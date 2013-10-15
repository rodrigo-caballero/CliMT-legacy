#!/contrib/bin/perl
				
# Purpose: Perform CRM distributions
# Script relies heavily on SSH connectivity between $HOST, $CVSROOT, and $ftp_mch

# Usage:
# $HOME/crm/crm/bld/crm_dst.pl ccm3_6_brnchT_crm2_1_2
# $HOME/crm/crm/bld/crm_dst.pl --dbg=1 ccm_brnch_crm 
# $HOME/crm/crm/bld/crm_dst.pl --dbg=2 --cln ccm_brnch_crm 
# $HOME/crm/crm/bld/crm_dst.pl --dbg=1 --cln --dst_cln ccm3_6_brnchT_crm2_1_2

# scp $HOME/crm/crm/bld/crm_dst.pl goldhill.cgd.ucar.edu:/home/zender/crm/crm/bld/crm_dst.pl

BEGIN{
    unshift @INC,$ENV{'HOME'}.'/perl'; # Location of DBG.pm HaS98 p. 170
} # end BEGIN

my $CVS_Header='$Header: /Users/rca/cvsroot/CliMT/src/radiation/ccm3/src/crm-2.1.2-ccm-3.6/bld/crm_dst.pl,v 1.1 2004/09/07 15:20:25 rca Exp $';

# Specify modules
use strict; # Protect all namespaces
use Getopt::Long; # GNU-style getopt
use File::Basename; # Parse filenames

# 3rd party modules

# Personal modules
require 'csz.pl'; # Personal library: cmd_prc(), date_time(), YYYYMMDD(), ...

# Set output flushing to help debugging on hard crashes. 
# These options update the filehandle after every output statement.
# See Camel book, p. 110.
select((select(STDOUT),$|=1)[0]);
select((select(STDERR),$|=1)[0]);

# Timing information
my $lcl_date_time=time_srt();

# Declare local variables
my ($idx,$rcd);
my ($prg_nm,$prg_dsc,$prg_vrs,$prg_date);
my ($pth_in,$fl_sfx);

my ($dst_vrs,$dst_fl);
my ($ccm_vrs,$crm_vrs);
my ($ccm_vrs_mjr,$ccm_vrs_mnr,$ccm_vrs_pch);
my ($crm_vrs_mjr,$crm_vrs_mnr,$crm_vrs_pch);
my ($dly_snp);
my ($mk_cmd,$tar_cmd);
my ($rsh_cmd,$rcp_cmd,$cp_cmd,$rm_cmd,$mkdir_cmd,$cvs_cmd);

# Set defaults 
my $False=0;
my $True=1;

my $CVS_Date='$Date: 2004/09/07 15:20:25 $';
my $CVS_Id='$Id: crm_dst.pl,v 1.1 2004/09/07 15:20:25 rca Exp $';
my $CVS_Revision='$Revision: 1.1 $';
my $CVSROOT='$CVSROOT'; # CVS repository
my $HOME=$ENV{'HOME'};
my $HOST=$ENV{'HOST'};
my $PVM_ARCH=$ENV{'PVM_ARCH'};
my $ccm_sng='ccm';
my $cp_cmd='cp -p -f'; # Command that behaves like cp
my $crm_sng='crm';
my $cvs_cmd='cvs -t'; # Command that behaves like cvs
my $data_nm=$ENV{'DATA'};
my $ftp_drc='/ftp/pub/zender/crm'; # Directory on FTP machine where repository resides
my $ftp_mch='ftp.cgd.ucar.edu'; # Machine where FTP repository resides
my $main_trunk_tag='ccm_brnch_crm';
my $mkdir_cmd='mkdir -p'; # Command that behaves like mkdir
my $rm_cmd='rm -f'; # Command that behaves like rm
my $rcp_cmd='scp -p'; # Command that behaves like rcp
my $rsh_cmd='ssh'; # Command that behaves like rsh
my $usr_nm=$ENV{'USER'};
my $vrs_tag=$main_trunk_tag;
my $www_drc='/web/web-data/cms/crm'; # WWW directory for package

# Set defaults for command line arguments
my $cln=$True; # GNU standard Makefile option `clean'
my $dbg_lvl=0;
my $dst_cln=$True; # GNU standard Makefile option `distclean'

# Derived fields
if($PVM_ARCH =~ m/SUN/){ # See Camel p. 81 for =~ and m//
    $tar_cmd='gtar';
    $mk_cmd='gmake';
}elsif($PVM_ARCH =~ m/CRAY/){
    $tar_cmd='tar';
    $mk_cmd='gnumake';
}else{
    $tar_cmd='tar';
    $mk_cmd='make';
} # endelse
if($data_nm eq ''){$data_nm='/data/'.$usr_nm;}
my $dst_pth_pfx=$data_nm; # Parent of build directory
if($dst_pth_pfx eq $HOME){die "$prg_nm: ERROR \$dst_pth_pfx eq $dst_pth_pfx";} # This could be disastrous
if($rm_cmd =~ m/( -r)|( -R)|( --recursive)/){die "$prg_nm: ERROR Dangerous setting \$rm_cmd eq $rm_cmd";} # This would be disastrous
if($HOST =~ m/cgd\.ucar\.edu/ || $HOST =~ m/sanitas/ || $HOST =~ m/goldhill/ || $HOST =~ m/dataproc/){
# CVS 1.10 has a bug where 'cvs -d :ext:user@host:repositorypath export -kkv -r revision_tag module' fails
# Workaround: Export from a machine cross-mounted to /fs/cgd so that :ext:user@host is not necessary
    $CVSROOT='/fs/cgd/csm/models/CVS.REPOS'; # CVS repository
}else{
    $CVSROOT=':ext:'.$usr_nm.'@goldhill.cgd.ucar.edu:/fs/cgd/csm/models/CVS.REPOS'; # CVS repository
} # endif CVSROOT

$prg_dsc='CRM distribution maker'; # Program description
($prg_nm,$prg_vrs)=$CVS_Id =~ /: (.+).pl,v ([\d.]+)/; # Program name and version
$prg_vrs.='*' if length('$Locker:  $ ') > 12; # Tack '*' if it is not checked in into CVS.
($prg_nm,$pth_in,$fl_sfx)=fileparse($0,''); # $0 is program name Camel p. 136.
if(length($CVS_Date) > 6){($prg_date)=unpack '@7 a19',$CVS_Date;}else{$prg_date='Unknown';}

# Parse command line arguments: '!' means Boolean, '|' is OR, '=' specifies required argument: 'i' is integer, 'f' is float, 's' is string
$rcd=Getopt::Long::Configure('no_ignore_case'); # Turn on case-sensitivity
$rcd=GetOptions( # man Getopt::GetoptLong
		'cln!' => \$cln,
		'clean!' => \$cln,
		'distclean!' => \$dst_cln,
		'dst_cln!' => \$dst_cln,
		'dbg_lvl=i' => \$dbg_lvl,
		); # end GetOptions()

# Parse positional arguments, if present
if($#ARGV > 0){die "$prg_nm: ERROR Called with $#ARGV+1 positional arguments, need no more than 1\n";}
elsif($#ARGV == 0){$vrs_tag=$ARGV[0];} # Version name is first positional argument, if present 

# Print initialization state
if($dbg_lvl >= 1){print ("$prg_nm: $prg_dsc, version $prg_vrs of $prg_date\n");} # endif dbg
if($dbg_lvl >= 2){print ("$prg_nm: \$vrs_tag = $vrs_tag\n");} # endif dbg
if($dbg_lvl >= 2){print ("$prg_nm: \$dbg_lvl = $dbg_lvl\n");} # endif dbg
if($dbg_lvl >= 2){print ("$prg_nm: \$cln = $cln\n");} # endif dbg
if($dbg_lvl >= 2){print ("$prg_nm: \$dst_cln = $dst_cln\n");} # endif dbg
if($dbg_lvl >= 2){print ("$prg_nm: \$tar_cmd = $tar_cmd\n");} # endif dbg

if($vrs_tag eq $main_trunk_tag){$dly_snp=$True;}else{$dly_snp=$False;}
# CRM is distributed using `cvs export' command, so version tag to be distributed must be supplied to this script
if($dly_snp){
# Version tag is of the form `ccm_brnch_crm'
    $crm_vrs=YYYYMMDD();
    $dst_vrs=$crm_sng.'-'.$crm_vrs;
}else{
# Version tag is of the form `ccm3_5_22_brnchT_crm2_1'
    my ($tag_sng,$brnch_sng);
    my ($brnch_psn,$ccm_psn,$crm_psn);

    $tag_sng=$vrs_tag;
    $tag_sng=~s/_/./g; # Convert underscores to periods
    $brnch_sng='brnch'; # Regex which separates CCM from CRM portion of tag
    $brnch_psn=index($tag_sng,$brnch_sng);
    $ccm_vrs=substr($tag_sng,length($ccm_sng),$brnch_psn-length($ccm_sng)-1);
    $crm_psn=index($tag_sng,$crm_sng);
    $crm_vrs=substr($tag_sng,$crm_psn+length($crm_sng),length($tag_sng)-$crm_psn+length($crm_sng));
    ($ccm_vrs_mjr,$ccm_vrs_mnr,$ccm_vrs_pch)=split(/\./,$ccm_vrs);
    ($crm_vrs_mjr,$crm_vrs_mnr,$crm_vrs_pch)=split(/\./,$crm_vrs);
    $dst_vrs=$crm_sng.'-'.$crm_vrs.'-'.$ccm_sng.'-'.$ccm_vrs;
    if($ccm_vrs_mjr != 3 || $crm_vrs_mjr < 1){die "$prg_nm: ERROR $ccm_vrs_mjr != 3 || $crm_vrs_mjr < 1"};
} # endelse
$dst_fl=$dst_vrs.'.tar.gz';
my $dst_pth_bld=$dst_pth_pfx.'/'.$dst_vrs; # Build directory

if($dbg_lvl >= 1){		 
    print STDOUT "$prg_nm: Version to release: $vrs_tag\n";
    print STDOUT "$prg_nm: Distribution version: $dst_vrs\n";
    print STDOUT "$prg_nm: Distribution file: $dst_fl\n";
    print STDOUT "$prg_nm: CCM version: $ccm_vrs\n";
    print STDOUT "$prg_nm: CCM major version: $ccm_vrs_mjr\n";
    print STDOUT "$prg_nm: CCM minor version: $ccm_vrs_mnr\n";
    print STDOUT "$prg_nm: CCM patch version: $ccm_vrs_pch\n";
    print STDOUT "$prg_nm: CRM version: $crm_vrs\n";
    print STDOUT "$prg_nm: CRM major version: $crm_vrs_mjr\n";
    print STDOUT "$prg_nm: CRM minor version: $crm_vrs_mnr\n";
    print STDOUT "$prg_nm: CRM patch version: $crm_vrs_pch\n";
} # end if dbg 

# Equivalent command line Perl command to convert version tag to version number
# -n causes Perl to loop over the input file(s)
# /contrib/bin/perl -n -e 'if(/ccm/){chop;s/_/./g;$ccm_sng="ccm";$crm_sng="crm";$tag_sng=$_;$brnch_psn=index($tag_sng,"brnch");$ccm_vrs=substr($tag_sng,3,$brnch_psn-4);$crm_psn=index($tag_sng,$crm_sng);$crm_vrs=substr($tag_sng,$crm_psn+3,length($tag_sng)-$crm_psn+3);$dst_vrs=$crm_sng."-".$crm_vrs."-".$ccm_sng."-".$ccm_vrs;print $dst_vrs;}' TAG >! VERSION

# fxm: improve CRM build validation
my $data_pth=$HOME.'/crm/crm/data';
chdir $data_pth or die "$prg_nm: ERROR unable to chdir to $data_pth: $!\n"; # $! is system error string
#cmd_prc('../bin/crm < mls_clr.in > mls_clr.out'); 
#cmd_prc('../bin/crm < mls_cld.in > mls_cld.out'); 

# Build distribution
chdir $dst_pth_pfx or die "$prg_nm: ERROR unable to chdir to $dst_pth_pfx: $!\n"; # $! is system error string
cmd_prc("$rm_cmd -r $dst_pth_bld"); # Remove contents of current directory, if any
cmd_prc("$mkdir_cmd $dst_pth_bld");
cmd_prc("$cvs_cmd -d $CVSROOT export -kkv -r $vrs_tag -d $dst_vrs crm"); # Export does not allow absolute paths in -d drc arguments for security reasons
cmd_prc("printf $dst_vrs > $dst_pth_bld/doc/VERSION"); # Stamp version in VERSION file

# Set up FTP server
chdir $dst_pth_pfx or die "$prg_nm: ERROR unable to chdir to $dst_pth_pfx: $!\n"; # $! is system error string
cmd_prc("$tar_cmd -cvzf $dst_fl ./$dst_vrs"); # Create gzipped tarfile
cmd_prc("$rsh_cmd $ftp_mch $rm_cmd $ftp_drc/$dst_fl"); # Remove any distribution with same name
if($dly_snp){cmd_prc("$rsh_cmd $ftp_mch $rm_cmd -r $ftp_drc/crm-????????.tar.gz");} # Remove previous daily snapshots from FTP server
cmd_prc("$rcp_cmd $dst_fl $ftp_mch:$ftp_drc"); # Copy local tarfile to FTP server
cmd_prc("$rcp_cmd $HOME/crm/crm/data/mls_clr.out $ftp_mch:$ftp_drc/mls_clr.out"); # Copy sample text output file to FTP server
cmd_prc("$rcp_cmd $HOME/crm/crm/data/crm.nc $ftp_mch:$ftp_drc/crm.nc"); # Copy sample netCDF output to FTP server

# Full release procedure (public releases only) includes update Web pages
if(!$dly_snp){
    cmd_prc("$rsh_cmd $ftp_mch $rm_cmd $ftp_drc/crm.tar.gz");
    cmd_prc("$rsh_cmd $ftp_mch \"cd $ftp_drc; ln -s $dst_fl crm.tar.gz\"");
    cmd_prc("$rcp_cmd $dst_pth_bld/doc/index.shtml $www_drc/index.shtml");
    cmd_prc("$rcp_cmd $dst_pth_bld/doc/README $www_drc/README");
    cmd_prc("$rcp_cmd $dst_pth_bld/doc/FAQ $www_drc/FAQ");
    cmd_prc("$rcp_cmd $dst_pth_bld/doc/INSTALL $www_drc/INSTALL");
    cmd_prc("$rcp_cmd $dst_pth_bld/doc/VERSION $www_drc/VERSION");
    cmd_prc("$rcp_cmd $dst_pth_bld/doc/ChangeLog $www_drc/ChangeLog");
} # endif 

# Housekeeping
if($cln){cmd_prc("$rm_cmd $dst_pth_pfx/$dst_fl");} # Remove local tarfile
if($dst_cln){cmd_prc("$rm_cmd -r $dst_pth_bld");} # Remove local distribution

# Sanity check
cmd_prc("$rsh_cmd $ftp_mch ls -l $ftp_drc");
