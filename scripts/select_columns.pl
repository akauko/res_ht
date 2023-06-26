#!/usr/bin/perl
use strict;

#This script selects columns by name from csv and tsv files
#Columns can be provided either as command line parameters or as a separate file
#Purpose is to preprocess very large files that R cannot easily open

#Tarkista muuttujien lukumäärä komentorivillä ja kerro usage, jos väärin
if ($#ARGV < 2) {
    print "Usage: perl select_columns.pl <input file> <output file> <columns>  \n";
    print "examples:	perl select_columns.pl finngen_R4_endpoint finngen_R4_HT ht_colnames.txt\n";
    print "		perl select_columns.pl finngen_R4_endpoint finngen_R4_HT FINNGENID I9_HYPTENS\n";
    exit;
}

my $infile= shift @ARGV;
my $outfile= shift @ARGV; 
my $sep = "\t";   #Separator


#Avaa inputfile
open(IN, "<$infile") || die "Can't open file $infile!\n";

#Nappaa inputfilen eka rivi talteen ja pilkotaan vektoriksi
chomp(my $title = <IN>); 
my @allcolnames_v = split $sep, $title;

#print "title as vect: @allcolnames_v";

#Muuta hashiksi: avain on nimi ja arvo on indeksi
my %allcolnames;
my $index=0;
foreach my $colname (@allcolnames_v){
	$allcolnames{$colname} = $index;
	$index++;
}
#print "title as hash: @{[ %allcolnames]}\n";

my @outcolnames;
if($#ARGV == 0) { #Kolme komentoriviparametriä
	my  $name = $ARGV[0];
	if(exists($allcolnames{$name})){ #..löytyy  sarakkeiden nimistä
		@outcolnames = ($name); #Ainoaksi alkioksi kolmas komentoriviparametri
	}
	else{  #Jos ei ole sarakkeen  nimi, sitten pitäisi olla tiedoston nimi
		open(IN2, "<$name") || die "$name is not a column name nor a file name!\n";
		chomp(@outcolnames = <IN2>);  #Luetaan tiedoston rivit muuttujaan.
	}
}
 
if($#ARGV > 0) { #Neljä tai enemmän komentoriviparametrejä
	@outcolnames = @ARGV;
	foreach  my $outcolname(@outcolnames){ #Loput argumenttivektorista sarakenimiksi
		if(!exists($allcolnames{$outcolname})){ #Tarkistetaan löytyvätkö inputtiedostosta
			die "Column $outcolname not found from file $infile!"
		}
	}
}
#print "outcolnames: @outcolnames\n";

#Muutetaan valittujen sarakkeiden nimet indekseiksi
my @selected_indices;
foreach my $outcolname(@outcolnames){
	push @selected_indices, $allcolnames{$outcolname};
}
#print "selected indices: @selected_indices\n";

#Vihdoinkin aloiteteaan kirjoittaamaan tulostiedostoa...
#Avaa outputfile
open(OUT, ">$outfile") || die "Can't open file!\n";

print OUT join($sep, @outcolnames)."\n";
while (<IN>) {
	chomp(my $line = $_);
	my @infields =  split $sep, $line;  #Tässä on nyt rivi pilkottuna vektoriksi.
	#print "inputfields: @infields";
	my @outfields = @infields[@selected_indices];
	print OUT join($sep, @outfields)."\n";
}


