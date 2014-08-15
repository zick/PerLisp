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
my $sym_t = makeSym('t');
my $sym_quote = makeSym('quote');
my $sym_if = makeSym('if');
my $sym_lambda = makeSym('lambda');
my $sym_defun = makeSym('defun');
my $sym_setq = makeSym('setq');
my $sym_loop = makeSym('loop');
my $sym_return = makeSym('return');
my $loop_val = $kNil;

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

sub pairlis {
    my ($lst1, $lst2) = @_;
    my $ret = $kNil;
    while ($$lst1{tag} eq 'cons' && $$lst2{tag} eq 'cons') {
        $ret = makeCons(makeCons($$lst1{car}, $$lst2{car}), $ret);
        $lst1 = $$lst1{cdr};
        $lst2 = $$lst2{cdr};
    }
    return nreverse($ret)
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
    } elsif ($c eq $kQuote) {
        my ($elm, $next) = read1(substr($str, 1));
        return (makeCons($sym_quote, makeCons($elm, $kNil)), $next);
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

sub findVar {
    my ($sym, $env) = @_;
    while ($$env{tag} eq 'cons') {
        my $alist = $$env{car};
        while ($$alist{tag} eq 'cons') {
            if ($$alist{car}->{car} == $sym) {
                return $$alist{car};
            }
            $alist = $$alist{cdr};
        }
        $env = $$env{cdr};
    }
    return $kNil;
}

sub addToEnv {
    my ($sym, $val, $env) = @_;
    $$env{car} = makeCons(makeCons($sym, $val), $$env{car});
}

my $g_env = makeCons($kNil, $kNil);

sub eval1 {
    my ($obj, $env) = @_;
    if ($$obj{tag} eq 'nil' || $$obj{tag} eq 'num' || $$obj{tag} eq 'error') {
        return $obj;
    }
    elsif ($$obj{tag} eq 'sym') {
        my $bind = findVar($obj, $env);
        if ($bind == $kNil) {
            return makeError($$obj{data} . ' has no value');
        }
        return $$bind{cdr};
    }

    my $op = safeCar($obj);
    my $args = safeCdr($obj);
    if ($op == $sym_quote) {
        return safeCar($args);
    } elsif ($op == $sym_if) {
        my $c = eval1(safeCar($args), $env);
        if ($$c{tag} eq 'error') {
            return $c;
        } elsif ($c == $kNil) {
            return eval1(safeCar(safeCdr(safeCdr($args))), $env);
        }
        return eval1(safeCar(safeCdr($args)), $env);
    } elsif ($op == $sym_lambda) {
        return makeExpr($args, $env);
    } elsif ($op == $sym_defun) {
        my $expr = makeExpr(safeCdr($args), $env);
        my $sym = safeCar($args);
        addToEnv($sym, $expr, $g_env);
        return $sym;
    } elsif ($op == $sym_setq) {
        my $val = eval1(safeCar(safeCdr($args)), $env);
        if ($$val{tag} eq 'error') {
            return $val;
        }
        my $sym = safeCar($args);
        my $bind = findVar($sym, $env);
        if ($bind == $kNil) {
            addToEnv($sym, $val, $g_env);
        } else {
            $$bind{cdr} = $val;
        }
        return $val;
    } elsif ($op == $sym_loop) {
        return loop($args, $env);
    } elsif ($op == $sym_return) {
        $loop_val = eval1(safeCar($args), $env);
        return makeError('');
    }
    return apply(eval1($op, $env), evlis($args, $env), $env);
}

sub evlis {
    my ($lst, $env) = @_;
    my $ret = $kNil;
    while ($$lst{tag} eq 'cons') {
        my $elm = eval1($$lst{car}, $env);
        if ($$elm{tag} eq 'error') {
            return $elm;
        }
        $ret = makeCons($elm, $ret);
        $lst = $$lst{cdr};
    }
    return nreverse($ret)
}

sub progn {
    my ($body, $env) = @_;
    my $ret = $kNil;
    while ($$body{tag} eq 'cons') {
        $ret = eval1($$body{car}, $env);
        if ($$ret{tag} eq 'error') {
            return $ret
        }
        $body = $$body{cdr};
    }
    return $ret;
}

sub loop {
    my ($body, $env) = @_;
    while (1) {
        my $ret = progn($body, $env);
        if ($$ret{tag} eq 'error') {
            if ($$ret{data} eq '') {
                return $loop_val;
            }
            return $ret;
        }
    }
}

sub apply {
    my ($fn, $args, $env) = @_;
    if ($$fn{tag} eq 'error') {
        return $fn;
    } elsif ($$args{tag} eq 'error') {
        return $args;
    } elsif ($$fn{tag} eq 'subr') {
        return $$fn{data}->($args);
    } elsif ($$fn{tag} eq 'expr') {
        return progn($$fn{body},
                     makeCons(pairlis($$fn{args}, $args), $$fn{env}));
    }
    return makeError('noimpl');
}

sub subrCar {
    my ($args) = @_;
    return safeCar(safeCar($args));
}

sub subrCdr {
    my ($args) = @_;
    return safeCdr(safeCar($args));
}

sub subrCons {
    my ($args) = @_;
    return makeCons(safeCar($args), safeCar(safeCdr($args)));
}

sub subrEq {
    my ($args) = @_;
    my $x = safeCar($args);
    my $y = safeCar(safeCdr($args));
    if ($$x{tag} eq 'num' && $$y{tag} eq 'num') {
        if ($$x{data} == $$y{data}) {
            return $sym_t;
        }
        return $kNil;
    } elsif ($x == $y) {
        return $sym_t;
    }
    return $kNil;
}

sub subrAtom {
    my ($args) = @_;
    if (safeCar($args)->{tag} eq 'cons') {
        return $kNil;
    }
    return $sym_t;
}

sub subrNumberp {
    my ($args) = @_;
    if (safeCar($args)->{tag} eq 'num') {
        return $sym_t;
    }
    return $kNil;
}

sub subrSymbolp {
    my ($args) = @_;
    if (safeCar($args)->{tag} eq 'sym') {
        return $sym_t;
    }
    return $kNil;
}

sub subrAddOrMul {
    my ($fn, $init_val) = @_;
    return sub {
        my ($args) = @_;
        my $ret = $init_val;
        while ($$args{tag} eq 'cons') {
            if ($$args{car}->{tag} ne 'num') {
                return makeError('wrong type');
            }
            $ret = $fn->($ret, $$args{car}->{data});
            $args = $$args{cdr};
        }
        return makeNum($ret);
    };
}
my $subrAdd = subrAddOrMul(sub { return $_[0] + $_[1]; }, 0);
my $subrMul = subrAddOrMul(sub { return $_[0] * $_[1]; }, 1);

sub subrSubOrDivOrMod {
    my ($fn) = @_;
    return sub {
        my ($args) = @_;
        my $x = safeCar($args);
        my $y = safeCar(safeCdr($args));
        if ($$x{tag} ne 'num' || $$y{tag} ne 'num') {
            return makeError('wrong type');
        }
        return makeNum($fn->($$x{data}, $$y{data}));
    };
}
my $subrSub = subrSubOrDivOrMod(sub { return $_[0] - $_[1]; });
my $subrDiv = subrSubOrDivOrMod(sub { return $_[0] / $_[1]; });
my $subrMod = subrSubOrDivOrMod(sub { return $_[0] % $_[1]; });

addToEnv(makeSym('car'), makeSubr(\&subrCar), $g_env);
addToEnv(makeSym('cdr'), makeSubr(\&subrCdr), $g_env);
addToEnv(makeSym('cons'), makeSubr(\&subrCons), $g_env);
addToEnv(makeSym('eq'), makeSubr(\&subrEq), $g_env);
addToEnv(makeSym('atom'), makeSubr(\&subrAtom), $g_env);
addToEnv(makeSym('numberp'), makeSubr(\&subrNumberp), $g_env);
addToEnv(makeSym('symbolp'), makeSubr(\&subrSymbolp), $g_env);
addToEnv(makeSym('+'), makeSubr($subrAdd), $g_env);
addToEnv(makeSym('*'), makeSubr($subrMul), $g_env);
addToEnv(makeSym('-'), makeSubr($subrSub), $g_env);
addToEnv(makeSym('/'), makeSubr($subrDiv), $g_env);
addToEnv(makeSym('mod'), makeSubr($subrMod), $g_env);
addToEnv($sym_t, $sym_t, $g_env);

print '> ';
while (defined(my $line = <STDIN>)) {
    my ($exp, $_) = read1($line);
    my $str = printObj(eval1($exp, $g_env));
    print "$str\n";
    print '> ';
}
