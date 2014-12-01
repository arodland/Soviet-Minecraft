use 5.12.0;
use warnings;

package Soviet::Minecraft;
# ABSTRACT: in Soviet Minecraft, server op you!

use Moo;
use MooX::Options;

use HTTP::Body;
use JSON;
use List::Util qw(max);
use Path::Tiny;
use IO::Async;
use IO::Async::Loop;
use IO::Async::Process;
use IO::Async::Stream;
use IO::Async::Timer::Countdown;
use Net::Async::HTTP::Server;
use curry;

option config_filename => (
  is => 'ro',
  default => 'soviet.json',
  format => 's',
  short => 'c',
  doc => 'config filename',
);

has json => (
  is   => 'ro',
  lazy => 1,
  default => sub { JSON->new->canonical->pretty; }
);

has config => (
  is    => 'ro',
  lazy  => 1,
  default => sub {
    my ($self) = @_;
    $self->json->decode( path( $self->config_filename )->slurp_raw ),
  },
);

sub save_config {
  my ($self) = @_;
  path($self->config_filename)->spew_raw(
    $self->json->encode( $self->config )
  );
}

has loop => (
  is => 'ro',
  init_arg => undef,
  default => sub { IO::Async::Loop->new },
);

has server => (
  is   => 'rw',
  init_arg => undef,
  clearer  => 'clear_server',
);

has minecraft_jar => (
  is => 'ro',
  lazy => 1,
  default => sub {
    my ($self) = @_;
    $self->config->{minecraft_jar} || 'minecraft_server.jar';
  },
);

sub _line_reader {
  my ($self, $method) = @_;

  return sub {
    my $self = shift;
    my ($buffref, $eof) = @_;

    while ($$buffref =~ s/^(.*)\n//) {
      $self->$method($1);
    }
    return 0;
  }
}

sub _setup_server {
  my ($self) = @_;

  my $server = IO::Async::Process->new(
    command => [
      qw(java -Xms1024M -Xmx1024M -jar), $self->minecraft_jar, qw(nogui),
    ],
    stdin => { via => 'pipe_write' },
    stdout => { on_read => $self->_line_reader('got_child_stdout') },
    stderr => { on_read => $self->_line_reader('got_child_stderr') },
    on_finish => $self->curry::got_child_close,
  );

  $self->server($server);
  $self->loop->add($server);
}

has stdin => (
  is => 'rw',
  init_arg => undef,
  clearer => 'clear_stdin',
);

sub _setup_stdin {
  my ($self) = @_;
  my $stdin = IO::Async::Stream->new(
    read_handle => \*STDIN,
    write_handler => \*STDERR, # never used
    on_read => $self->_line_reader('got_console_stdin'),
  );

  $self->stdin($stdin);
  $self->loop->add($stdin);
}

has httpd => (
  is   => 'ro',
  lazy => 1,
  clearer => 'clear_httpd',
  builder => '_build_httpd',
);

has httpd_port => (is => 'ro', lazy => 1, default => sub { 8181 });

sub _build_httpd {
  my ($self) = @_;

  my $server = Net::Async::Server::HTTP::PSGI->new(
    app => sub {
      my $env = shift;
      my $req = Plack::Request->new($env);
      my $res = $req->new_response;
      $res->header(Server => 'Synergy');

      if ($req->path eq '/sms') {
        return $self->_http_sms($env, $req, $res);
      } else {
        return $self->_http_404($env, $req, $res);
      }
    }
  );
  $self->loop->add($server);
  $server->listen(
    addr => { family => 'inet6', socktype => 'stream', port => $self->httpd_port },
    on_listen_error => sub { die "Cannot listen: $_[-1]" },
  );
  return $server;
}

sub _http_sms {
  my ($self, $env, $request, $response) = @_;

  my $param = $request->parameters;

  my $from = $param->{From} // '';
  $from =~ s/\A\+1//;

  my $sender_ok = $self->config->{allow_sms_from}->{$from};
  unless ($param->{AccountSid} eq $self->config->{twilio_sid} and $sender_ok) {
    $response->status(400);
    $response->body('Bad request');
    warn sprintf "Bad request for %s from phone %s from IP %s",
      $request->path . '?' . $request->query_string,
      $from,
      $request->address;
    return $response->finalize;
  }

  my $text = lc $param->{Body};

  my $reply;
  my $command;

  if ($text =~ /\Aallow ([-_0-9a-z]+)\z/i) {
    my $who = $1;
    $command = "whitelist add $who";
    $reply   = "Okay, I've added $who to the whitelist!";
  } elsif ($text eq 'emergency shutdown' or $text eq 'emergency shut down') {
    $command = "stop";
    $reply   = "I'm issuing an emergency shutdown!";
  }

  if (defined $reply) {
    $response->status(200);
    $response->body($reply);
  } else {
    $response->status(200);
    $response->body("Does not compute.");
  }

  warn("Request from " . $request->address . " " . $request->path . "?" . $request->query_string);

  if (defined $reply and length $command) {
    $self->server->stdin->write($command . "\n");
  }


  return $response->finalize;
}

sub _http_404 {
  my ($self, $env, $request, $response) = @_;

  $response->code(404);
  $response->content(
    "Hi visitor from "
    . $request->address
    . ", Page not found -> '"
    . $request->path . "'\n\n"
  );

  warn
    "Request from "
    . $request->address . " " . $request->path . '?' . $request->query_string;
}

sub run {
  my ($self) = @_;
  $self->_setup_server;
  $self->_setup_stdin;

  $self->httpd;

  $self->loop->watch_child($self->server->pid, $self->curry::got_child_signal);

  print "Child pid ", $self->server->pid, " started.\n";
  
  $self->loop->run;
}

has election => (
  is => 'rw',
  lazy => 1,
  default => sub { +{} },
);

sub got_cast_vote {
  my ($self, $who, $which, $vote) = @_;
  
  # Election in progress?
  #   YES: add/change vote
  #   NO : Last election very recent?
  #     YES: Complain and refuse.
  #     NO : Begin new election.
  my $election = $self->election->{$which} ||= {};

  if ($election->{completed_at}) {
    if (
      keys %{ $election->{votes} } > 1
      and
      $election->{completed_at} > time - 300
    ) {
      $self->server->stdin->write("msg $who You can't change the $which again so soon!\n");
      return;
    }

    $election = $self->election->{$which} = {};
  }

  if (! $election->{began_at}) {
    $election->{began_at} = time;
    $self->loop->add(
      IO::Async::Timer::Countdown->new(
        delay => 31,
        on_expire => $self->curry::election_complete($which),
      )
    );
  }

  $election->{votes}{$which} = $vote;
  $self->server->stdin->write("list\n"); # to trigger early termination
}

sub got_updated_player_count {
  my ($self, $curr, $max) = @_;
  for my $which (keys %{ $self->election }) {
    my $votes = values %{ $self->election->{$which}{votes} };
    if ($votes >= $curr) {
      $self->loop->later( $self->curry::election_complete($which) );
    }
  }
}

sub election_complete {
  my ($self, $which) = @_;

  my $server = $self->server;
  my $election = $self->election->{$which};

  if ($election->{completed_at}) {
    # This happens when the on-delay event fires after the election completed
    # because of a player count update. -- rjbs, 2014-11-28
    return;
  }

  $election->{completed_at} = time;

  my @votes    = values %{ $election->{votes} };

  my %score;
  $score{$_} ++ for values %{ $election->{votes} };
  my @ranked = sort { $score{$b} <=> $score{$a} } keys %score;
  if (@votes > 1 && $score{ $ranked[0] } == $score{ $ranked[1] }) {
    # n-way tie, no change?
    $server->stdin->write("say The $which vote was a tie.  Nothing will change.\n");
    return;
  }

  my $winner = $ranked[0];

  if ($which eq 'rain') {
    if    ($winner eq 'on')   { $server->stdin->write("weather rain\n") }
    elsif ($winner eq 'off')  { $server->stdin->write("weather clear\n") }
    elsif ($winner eq 'hard') { $server->stdin->write("weather thunder\n") }

  } elsif ($which eq 'time') {
    if    ($winner eq 'sunrise') { $server->stdin->write("time set day\n") }
    elsif ($winner eq 'sunset')  { $server->stdin->write("time set night\n") }

  } else {
    warn "don't know how to handle $which election!!";
  }
}

has tp => (
  is => 'rw',
  lazy => 1,
  default => sub { +{} },
);

sub got_xyz_teleport {
  my ($self, $who, $tp) = @_;

  return unless my $callbacks = $self->tp->{who};

  for my $key (keys %$callbacks) {
    my $callback = delete $callbacks->{$key};
    $callback->{code}->(@_) unless time > $callback->{expires_at};
  }
}

sub hub_xyz {
  my ($self) = @_;
  $self->config->{hub}
}

sub home_for {
  my ($self, $player) = @_;

  $self->config->{home}{$player} || $self->hub_xyz;
}

sub porch_for {
  my ($self, $player) = @_;
  $self->config->{porch}{$player} || $self->home_for($player);
}

sub got_child_stdout {
  my ($self, $stdout_line) = @_;
  my $server = $self->server;
  # print "pid ", $self->server->pid, " STDOUT: $stdout_line\n";
  print "$stdout_line\n";

  # [11:01:21] [Server thread/INFO]: rjbs joined the game
  # [11:01:28] [Server thread/INFO]: <rjbs> go home
  return unless my $parse = naive_parse($stdout_line);

  if (my $tp = tp_parse($parse->{message})) {
    $self->loop->later($self->curry::got_xyz_teleport(lc $tp->{who}, $tp));
    return;
  }

  if (
    my ($curr, $max) =
      $parse->{message} =~ m{\AThere are ([0-9]+)/([0-9]+) players}
  ) {
    $self->loop->later($self->curry::got_updated_player_count($curr, $max));
    return;
  }

  if (my ($who, $what) = $parse->{message} =~ /\A<([^>]+)>\s+(.+)\z/) {
    $who  = lc $who;
    $what = lc $what;

    if    ($what eq '!hub')     { $server->stdin->write("tp $who " . $self->hub_xyz) }
    elsif ($what eq '!home')    { $server->stdin->write("tp $who " . $self->home_for($who)); }

    elsif ($what eq '!set home') {
      $self->tp->{$who}{home} = {
        expires_at => time + 5,
        code       => sub {
          my ($self, $tp) = @_;
          $self->config->{home}{$who} = "$tp->{x} $tp->{y} $tp->{z}";
          $self->loop->later( $self->curry::save_config );
          $server->stdin->write("msg $who Your home has been updated.\n");
        },
      };
      $server->put("tp $who ~ ~ ~");
    }

    elsif ($what eq '!set porch')  {
      $self->tp->{$who}{porch} = {
        expires_at => time + 5,
        code       => sub {
          my ($self, $tp) = @_;
          $self->config->{porch}{$who} = "$tp->{x} $tp->{y} $tp->{z}";
          $self->loop->later( $self->curry::save_config );
          $server->stdin->write("msg $who Your front porch location has been updated.");
        },
      };
    }

    elsif ($what eq '!sunrise') {
      $self->loop->later( $self->curry::got_cast_vote($who, 'time', 'sunrise') );
      $server->stdin->write("msg $who You cast your vote for sunrsie.");
    }
    elsif ($what eq '!sunset') {
      $self->loop->later( $self->curry::got_cast_vote($who, 'time', 'sunset') );
      $server->stdin->write("msg $who You cast your vote for sunset.");
    }

    elsif ($what eq '!rain on') {
      $self->loop->later( $self->curry::got_cast_vote($who, 'rain', 'on') );
      $server->stdin->write("msg $who You cast your vote for rain.");
    }
    elsif ($what eq '!rain off') {
      $self->loop->later( $self->curry::got_cast_vote($who, 'rain', 'off') );
      $server->stdin->write("msg $who You cast your vote for clear skies.");
    }
    elsif ($what eq '!rain hard') {
      $self->loop->later( $self->curry::got_cast_vote($who, 'rain', 'hard') );
      $server->stdin->write("msg $who You cast your vote for a thunderstorm.");
    }

    elsif ($what =~ /\A!visit (\S+)\z/) {
      $server->stdin->write("tp $who " . $self->porch_for($1) );
    }

    elsif ($what =~ /\A!mode (creative|survival)\z/i) {
      my $mode = lc($1) eq 'creative' ? 1 : 0;
      $server->stdin->write("gamemode $mode $who");
    }

    elsif ($what =~ /\A!join (\S+)\z/) {
      # If $1 isn't a player, report an error.
      $server->stdin->write("tp $who $1");
    }
  }
}

sub tp_parse {
  my ($msg) = @_;

  # [22:15:01] [Server thread/INFO]:
  # Teleported rjbs to 688.330745199867, 75.0, 479.96637932110707
  
  my ($who, $x, $y, $z) = $msg =~ /\A
    Teleported \s (\S+) \s to \s
    ([0-9]+\.[0-9]+), \s
    ([0-9]+\.[0-9]+), \s
    ([0-9]+\.[0-9]+)
  \z/x;

  return unless defined $who;
  return { who => lc $who, x => $x, y => $y, z => $z };
}

sub naive_parse {
  my ($line) = @_;
  my ($ts, $channel, $rest) = $line =~ /
    \A
    \[([0-9]{2}:[0-9]{2}:[0-9]{2})\]
    \s+
    \[([^\]]+)\]:
    \s+
    (.+)
  /x;

  return unless defined $ts;

  return {
    timestamp => $ts,
    channel   => $channel,
    message   => $rest,
  };
}

sub got_child_stderr {
  my ($self, $stderr_line) = @_;
  warn "$stderr_line\n";
}

sub got_child_close {
  my ($self) = @_;

  print "pid ", $self->server->pid, " closed all pipes.\n";

  $self->loop->remove($self->server);
  $self->clear_server;
  $self->loop->remove($self->stdin);
  $self->clear_stdin;
  $self->loop->remove($self->httpd);
  $self->clear_httpd;

  return;
}

sub got_child_signal {
  my ($self, $pid, $code) = @_;

  print "PID $pid exited with status $code.\n";

  # May have been reaped by on_child_close()
  return unless $self->server && $self->server->pid == $pid;

  $self->loop->remove($self->server);
  $self->clear_server;
}

sub got_console_stdin {
  my ($self, $input) = @_;

  $self->server->stdin->write("$input\n");
}

1;
