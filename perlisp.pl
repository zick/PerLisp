use strict;

my $kLPar = '(';
my $kRPar = ')';
my $kQuote = "'";
my $kNil = { tag => 'nil', data => 'nil' };

sub safeCar {
    my ($obj) = @_;
    if ($$obj{tag} eq 'cons') {
        return $$obj{car};
    }
    return $kNil;
}

sub safeCdr {
    my ($obj) = @_;
    if ($$obj{tag} eq 'cons') {
        return $$obj{cdr};
    }
    return $kNil;
}

sub makeError {
    my ($str) = @_;
    return { tag => 'error', data => $str }
}

my $sym_table = {};
sub makeSym {
    my ($str) = @_;
    if ($str eq 'nil') {
        return $kNil;
    }
    if (!exists $$sym_table{$str}) {
        $$sym_table{$str} = { tag => 'sym', data => $str };
    }
    return $$sym_table{$str};
}

sub makeNum {
    my ($num) = @_;
    return { tag => 'num', data => $num };
}

sub makeCons {
    my ($a, $d) = @_;
    return { tag => 'cons', car => $a, cdr => $d };
}

sub makeSubr {
    my ($fn) = @_;
    return { tag => 'subr', data => $fn };
}

sub makeExpr {
    my ($args, $env) = @_;
    return { tag => 'expr',
             args => safeCar($args),
             body => safeCdr($args),
             env => $env };
}

sub nreverse {
    my ($lst) = @_;
    my $ret = $kNil;
    while ($$lst{tag} eq 'cons') {
        my $tmp = $$lst{cdr};
        $$lst{cdr} = $ret;
        $ret = $lst;
        $lst = $tmp;
    }
    return $ret;
}

sub isDelimiter {
    my ($c) = @_;
    return $c eq $kLPar || $c eq $kRPar || $c eq $kQuote || $c =~ /\s+/;
}

sub skipSpaces {
    my ($str) = @_;
    $str =~ s/^\s+//;
    return $str;
}

sub makeNumOrSym {
    my ($str) = @_;
    if ($str =~ /^[+-]?\d+$/) {
        return makeNum($str + 0);
    }
    return makeSym($str);
}

sub readAtom {
    my ($str) = @_;
    my $next = '';
    for (my $i = 0; $i < length($str); $i++) {
        if (isDelimiter(substr($str, $i, 1))) {
            $next = substr($str, $i);
            $str = substr($str, 0, $i);
            last;
        }
    }
    return (makeNumOrSym($str), $next);
}

sub read1 {
    my ($str) = @_;
    $str = skipSpaces($str);
    if ($str eq '') {
        return (makeError('empty input'), '');
    }
    my $c = substr($str, 0, 1);
    if ($c eq $kRPar) {
        return (makeError('invalid syntax: ' . $str), '');
    } elsif ($c eq $kLPar) {
        return (readList(substr($str, 1)), '');
    } elsif ($c eq $kRPar) {
        return (makeError('noimpl'), '');
    }
    return readAtom($str);
}

sub readList {
    my ($str) = @_;
    my $ret = $kNil;
    for (;;) {
        $str = skipSpaces($str);
        if ($str eq '') {
            return (makeError('unfinished parenthesis'), '');
        } elsif (substr($str, 0, 1) eq $kRPar) {
            last;
        }
        my ($elm, $next) = read1($str);
        if ($$elm{tag} eq 'error') {
            return $elm;
        }
        $ret = makeCons($elm, $ret);
        $str = $next;
    }
    return (nreverse($ret), substr($str, 1));
}

sub printObj {
    my ($obj) = @_;
    if ($$obj{tag} eq 'num' || $$obj{tag} eq 'sym' || $$obj{tag} eq 'nil') {
        return $$obj{data};
    } elsif ($$obj{tag} eq 'error') {
        return '<error: ' . $$obj{data} . '>';
    } elsif ($$obj{tag} eq 'cons') {
        return printList($obj);
    } elsif ($$obj{tag} eq 'subr') {
        return '<subr>';
    } elsif ($$obj{tag} eq 'expr') {
        return '<expr>';
    }
    return '<unknown object>';
}

sub printList {
    my ($obj) = @_;
    my $ret = '';
    my $first = 1;
    while ($$obj{tag} eq 'cons') {
        if ($first) {
            $ret = printObj($$obj{car});
            $first = 0;
        } else {
            $ret .= ' ' . printObj($$obj{car});
        }
        $obj = $$obj{cdr};
    }
    if ($$obj{tag} eq 'nil') {
        return '(' . $ret . ')';
    }
    return '(' . $ret . ' . ' . printObj($obj) . ')';
}

while (defined(my $line = <STDIN>)) {
    my ($ret, $_) = read1($line);
    my $str = printObj($ret);
    print "$str\n";
}
