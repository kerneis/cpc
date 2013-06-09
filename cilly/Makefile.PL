use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'cilly',
    VERSION_FROM        => 'App/Cilly.pm',
    PM                  => {
        'App/Cilly.pm'      => '$(INST_LIBDIR)/App/Cilly.pm',
        'App/Cilly/CilConfig.pm' => '$(INST_LIBDIR)/App/Cilly/CpcConfig.pm',
        'App/Cilly/KeptFile.pm' => '$(INST_LIBDIR)/App/Cilly/KeptFile.pm',
        'App/Cilly/OutputFile.pm' => '$(INST_LIBDIR)/App/Cilly/OutputFile.pm',
        'App/Cilly/TempFile.pm' => '$(INST_LIBDIR)/App/Cilly/TempFile.pm',
    },
    EXE_FILES           => [ 'cpc' ],
    PREREQ_PM => {
        'Carp'              => 0,
        'Data::Dumper'      => 0,
        'File::Basename'    => 0,
        'File::Copy'        => 0,
        'File::Spec'        => 0,
        'File::Temp'        => 0,
        'FindBin'           => 0,
        'Getopt::Long'      => 0,
        'Text::ParseWords'  => 0,
    },
    PL_FILES            => {},
    MAN3PODS            => {}, # no need for man pages for any of the .pm files
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => '' },
);

package MY;

sub MY::top_targets {
    my $str = shift->SUPER::top_targets(@_);

    $str =~ s/^pure_all.+/$& fix-cilbindir cpc/m;

    return $str;
}

sub MY::postamble {
    my $postamble = <<'MAKE_FRAG';

.PHONY: fix-cilbindir

fix-cilbindir:
ifdef CILBINDIR
	perl -p -i -e "s,^.*cilbindir.*$$,\\$$::cilbindir = \"$(CILBINDIR)\";," $(INST_LIBDIR)/App/Cilly/CpcConfig.pm
endif

App/Cilly/CpcConfig.pm: App/Cilly/CilConfig.pm
	cp -f $< $@

cpc: cilly App/Cilly/CpcConfig.pm
	cp -f $< $@
	perl -p -i -e "s|cilbindir/cilly|cilbindir/cpc|;s|CilConfig|CpcConfig|" $@
	chmod 0555 $@

MAKE_FRAG

    return $postamble;
}

1;
