package R::Comment2Man;

use strict;
use English;
use File::Temp qw(tempfile);
use Data::Dumper;

our $VERSION = 0.3;

# entrance
sub draft {
    my $class = shift;
    my $dir = shift;
    my %settings = (overwrite => 0,
	                @_);
    
    my ($R_MERGE_FH, $r_merge_filename) = tempfile();
    
    # find all the R script
    my @r_files;
    if( -f $dir) {   # if only an R script
        @r_files = ($dir);
    } 
    elsif(-d $dir) {    # if it is a dir
        @r_files = glob("$dir/*.R");
    } 
    else {
        die "cannot find $dir.\n";
    }
    
    # merge all the R script into one file
    foreach my $r_file (@r_files) {
        print "- merge $r_file\n";
        
        open my $R_FH, "<", $r_file or die "cannot open $r_file.";
        print $R_MERGE_FH join "", <$R_FH>;
        print $R_MERGE_FH "\n\n";
        close $R_FH; 
    }
    
    close $R_MERGE_FH;
    
    print "R scripts are merged into $r_merge_filename\n";
    print "Now convert the comments\n";
	
	if(-e "man" and !(-d "man")) {
		print("Find available file with name 'man' in current directory.\nStop.\n");
		exit;
	}
	if(! -e "man") {
		mkdir("man");
	}
	
	# $data is a array reference in which each element
	# is a hash reference storing sections in the comment
    my $data = parse($r_merge_filename, $settings{overwrite});
    
    print "Remove temp files ($r_merge_filename)\n";
    unlink($r_merge_filename);
    
    print "Done, you doc drafts are in /man dir.\n\n";
}


sub parse {
	open my $R_FH, $_[0] or die $@;
	my $is_overwrite = $_[1];
	my @lines = <$R_FH>;
	
	my @items;
    for(my $i = 0; $i < scalar(@lines); $i ++) {
		my $line = $lines[$i];
		my $item;
		
		# documented item start with # ## title or something like that
		if($line =~/^#\s+[=@#*\-+$%&]{2}\s*title\s*$/) {
			$item = R::Comment2Man::Item->read(\@lines, $i, is_function => 1);
			push(@items, $item);
		} elsif($line =~/^#\s+[=@#*\-+$%&]{2}\s*title\s*\(\s*data:\s*(\S+)\s*\)/) {
			$item = R::Comment2Man::Item->read(\@lines, $i, is_function => 0);
			$item->{_function_name} = $1;
			$item->{_function_args} = "data($1)";
			push(@items, $item);
		} elsif($line =~/^#\s+[=@#*\-+$%&]{2}\s*title\s*\(\s*package:\s*(\S+)\s*\)/) {
			$item = R::Comment2Man::Item->read(\@lines, $i, is_function => 0);
			$item->{_function_name} = "$1-package";
			$item->{_function_args} = "package($1)";
			push(@items, $item);
		}
		
	}
	
	@items = sort { $a->{_function_name} cmp $b->{_function_name} } @items;
	
	my @parsed_items;
	open NAMESPACE, ">NAMESPACE";
	print NAMESPACE "export(\n";
	for(my $i = 0; $i < scalar(@items); $i ++) {
		my ($section_name, $section_value) = $items[$i]->parse()->format();
		if($is_overwrite == 0 and -e "man/$items[$i]->{_function_name}.rd") {
            my ($old_section_name, $old_section_value) = read_manfile_section("man/$items[$i]->{_function_name}.rd");
			foreach (@$old_section_name) {
				print "  update $_\n";
			}
			($section_name, $section_value) = combine_sections($section_name, $section_value, $old_section_name, $old_section_value);
		}
		output($items[$i]->{_function_name}, $section_name, $section_value);
		if($items[$i]->{_function_args} =~/^data\(/ or $items[$i]->{_function_args} =~/^package\(/) {
		
		} else {
			print NAMESPACE "\t$items[$i]->{_function_name}".($i == $#items ? "" : ",")."\n";
		}
		print "man/$items[$i]->{_function_name}.rd... done.\n\n";
	}
	print NAMESPACE ")\n";
	close NAMESPACE;
}

# alignment of two vectors of section name
sub combine_sections {
	my $section_name = shift;
	my $section_value = shift;
	my $old_section_name = shift;
	my $old_section_value = shift;
	
	my $tr = R::Comment2Man::Align::align_score_matrix($section_name, $old_section_name);
	my ($align1, $align2) = R::Comment2Man::Align::align_traceback($section_name, $old_section_name, $tr);
	
	my $name;
	my $value;
	for(my $i = 0; $i < scalar(@$align1); $i ++) {
		if($align1->[$i]) {
			$name->[$i] = $section_name->[$i];
			$value->[$i] = $section_value->[$i];
		} elsif($align2->[$i]) {
			$name->[$i] = $old_section_name->[$i];
			$value->[$i] = $old_section_value->[$i];
		}
	}
	
	return($name, $value);
}

# write to the man file
sub output {
	my $function_name = shift;
	my $section_name = shift;
	my $section_value = shift;
	
	open my $fh, ">man/$function_name.rd";
	for(my $i = 0; $i < scalar(@$section_name); $i ++) {
		if($section_name->[$i] eq "usage" and $section_value->[$i] =~/^package\(/) {
			next;
		}
		if($section_name->[$i] eq "name" or 
		   $section_name->[$i] eq "docType") {
			print $fh "\\$section_name->[$i]"."{";
			print $fh "$section_value->[$i]";
			print $fh "}\n";
		} elsif($section_name->[$i] eq "alias") {
		
			if(ref($section_value->[$i]) eq "ARRAY") {
				foreach (@{$section_value->[$i]}) {
					print $fh "\\$section_name->[$i]"."{";
					print $fh "$_";
					print $fh "}\n";
				}
			} else {
				print $fh "\\$section_name->[$i]"."{";
				print $fh "$section_value->[$i]";
				print $fh "}\n";
			}
		} else {
			print $fh "\\$section_name->[$i]"."{\n";
			print $fh "$section_value->[$i]\n";
			print $fh "}\n";
		}
	}
	close $fh;
}

# some man file may exist
sub read_manfile_section {
    open F, $_[0];

	my $text = join "", <F>;

	my $m;
	$m = qr/
			\{
			  (?:
				[^{}]+
			   |
				 (?:(??{$m}))
			   )*
			 \}
			/x;

	my @a = $text =~ /\\(\w+)\s*($m)/gms;
	close F;
	
	my $section_name = [];
	my $section_value = [];
	for(my $i = 0; $i < scalar(@a); $i += 2) {
		push(@$section_name, $a[$i]);
		
		$a[$i + 1] =~s/^\{|\}$//g;
		$a[$i + 1] =~s/^\s*|\s*$//gs; # removing leading/tracing white space characters
		push(@$section_value, $a[$i+1]);;
	}
	
	return($section_name, $section_value);
}



# comments for single R function
package R::Comment2Man::Item;
use English;
use Data::Dumper;

# transform the wrong section names to right names
sub check_synonyms {
	my %s = ("desc"       => "description",
                 "parameters" => "arguments",
                 "param"      => "arguments",
                 "args"       => "arguments",
                 "arg"        => "arguments",
                 "return"     => "value",
                 "values"     => "value",
                 "reference"  => "references",
                 "ref"        => "references",
                 "detail"     => "details",
                );
	if($s{lc($_[0])}) {
		return $s{lc($_[0])};
	} else {
		return lc($_[0]);
	}
}

# only read line records for correponding sections
# in these record, there are no '^#"
sub read {
	my $class = shift;
	
	my $lines_ref = shift;
	my $index = shift;
	my $is_function = {@_}->{is_function};
	
	my $dl = [];
	for my $i ("a".."z") {
		for my $j ("a".."z") {
			push(@$dl, "$i$j");
		}
	}
	
	my $sections;
	my $current_section;
	for(my $i = $index; $i < scalar(@$lines_ref); $i ++) {
		my $line = $lines_ref->[$i];
		
		if($line =~/^#/) {
			if($line =~/^#\s+[@#=*\-+$%&]{2}\s*(\w+)/) {
				$current_section = $1;
				$current_section = check_synonyms($current_section);
				$current_section = shift(@$dl)."_".$current_section;
				$sections->{$current_section} = [];
			} else {
				$line =~s/^#\s?//s;
				$line =~s/^\s+$//g;
				push(@{$sections->{$current_section}}, $line);
			}
		} elsif($is_function) {
			my @res = get_nearest_function_info($lines_ref, $i);
			$sections->{_function_name} = $res[0];
			$sections->{_function_args} = $res[1];
			last;
		} else {
			last;
		}
	}
	bless $sections, $class;
	return $sections;
}

# wrapper, convert the comment to a simple tree
sub parse {
	my $self = shift;
	
	my $parsed;
	foreach my $key (keys %$self) {
		if ($key =~/^_/) {
			$parsed->{$key} = $self->{$key};
			next;
		}
		
		$parsed->{$key} = convert_to_tree($self->{$key});
	}
	bless $parsed, ref($self);
	return $parsed;
}

# whether the comment has this section
sub has_section {
	my $hash = shift;
	my $section = shift;
	
	foreach my $keys (keys %$hash) {
		if($keys =~/$section/i) {
			return 1;
		}
	}
	return 0;
}

# format the comment tree to tex code
sub format {
	my $self = shift;
	
	my $str = "";
	my $section_name = [];
	my $section_value = [];
	
	push(@$section_name, "name");
	push(@$section_value, $self->{_function_name});
	push(@$section_name, "alias");
	if($self->{_function_args} =~/^package\(/) {
		push(@$section_value, "$self->{_function_name}");
	} else {
		push(@$section_value, "$self->{_function_name}");
	}
	
	if($self->{_function_args} =~/^data\(/) {
		push(@$section_name, "docType");
		push(@$section_value, "data");
	}
	if($self->{_function_args} =~/^package\(/) {
		push(@$section_name, "docType");
		push(@$section_value, "package");
	}
	
	my $last_section;
	my @res;
	foreach my $section (sort keys %$self) {
		next if($section =~/^_function/);
		
		if($last_section =~/title/i and !has_section($self, "description")) {
			@res = format_section($self, $last_section, "description");
			push(@$section_name, $res[0]);
			push(@$section_value, $res[1]);
			$last_section = "description";
		}
		
		if($last_section =~/description/i) {
			push(@$section_name, "usage");
			push(@$section_value, $self->{_function_args});
			$last_section = "usage";
		}
		
		$last_section = $section;
		
		@res = format_section($self, $section);
		push(@$section_name, $res[0]);
		push(@$section_value, $res[1]);
	}

	
	return ($section_name, $section_value);
}

# format each section
# in general, the block-level content can be categorized
# in "paragrpah", "named list", "simple list", "code chunk"
sub format_section {
	my $self = shift;
	my $section = shift;
	my $option_name = shift;
	
	my $str;
	my $section_name = $option_name ? $option_name : substr($section, 3);
		foreach my $k (sort keys %{$self->{$section}}) {
			my $v = $self->{$section}->{$k};
			if($k =~/_paragraph/) {
				$str .= "  ".inline_format($v)."\n\n";
			} elsif($k =~/_named_item/) {
				if($section_name ne "arguments") {
					$str .= "\\describe{\n";
				}
				for(my $i = 0; $i < scalar(@{$v->{name}}); $i ++) {
					$str .= "  \\item{$v->{name}->[$i]}{".inline_format($v->{value}->[$i])."}\n";
				}
				if($section_name ne "arguments") {
					$str .= "}\n";
				}
			} elsif($k =~/_item/) {
				$str .= "  \\itemize{\n";
				for(my $i = 0; $i < scalar(@{$v}); $i ++) {
					$str .= "    \\item ".inline_format($v->[$i])."\n";
				}
				$str .= "  }\n";
			} elsif($k =~/_code_block/) {
				$str .= "  \\preformatted{\n";
				$v =~s/\{/\\{/g;
				$v =~s/\}/\\}/g;
				$str .= $v;
				$str .= "  }\n";
			}
		}

	return ($section_name, $str);
}

# something like url, code link, font ...
sub inline_format {
	my $str = shift;
	$str = trans_code($str);
	$str = trans_url($str);
	return $str;
}

# convert the comment to a comment tree (only one level)
sub convert_to_tree {
	my $lines_ref = shift;
	
	my $dl = [];
	for my $i ("a".."z") {
		for my $j ("a".."z") {
			push(@$dl, "$i$j");
		}
	}
	
	my $tree;
	for(my $i = 0; $i < scalar(@$lines_ref); $i ++) {
		if($lines_ref->[$i] eq "") {
			next;
		}
		
		my $h;
		if($lines_ref->[$i] =~/^-\s/) {
			($h, $i) = read_item($lines_ref, $i);
			$tree->{shift(@$dl)."_item"} = $h;
		} elsif($lines_ref->[$i] =~/^-\S+\s/) {
			($h, $i) = read_named_item($lines_ref, $i);
			$tree->{shift(@$dl)."_named_item"} = $h;
		} elsif($lines_ref->[$i] =~/^\s+\S/ and is_code_block($lines_ref, $i)) {
			($h, $i) = read_code_block($lines_ref, $i);
			$tree->{shift(@$dl)."_code_block"} = $h;
		} else {
			($h, $i) = read_paragraph($lines_ref, $i);
			$tree->{shift(@$dl)."_paragraph"} = $h;
		}
	}
	return $tree;
}

# list with no name
sub read_item {
	my $lines_ref = shift;
	my $index = shift;
	
	my $item;
	for(my $i = $index; $i < scalar(@$lines_ref); $i ++) {
		my $line = $lines_ref->[$i];
		chomp $line;
		if($lines_ref->[$i] =~/^-\s/) {
			$line =~s/^-\s+//;
			push(@$item, $line);
		} elsif($i == $#$lines_ref or $line eq "") {
			return ($item, $i);
		} else {
			$line =~s/^\s+//;
			$item->[$#$item] .= " $line";
		}
	}
}

# list with names
sub read_named_item {
	my $lines_ref = shift;
	my $index = shift;
	
	my $item = {name => [], value => []};
	for(my $i = $index; $i < scalar(@$lines_ref); $i ++) {
		my $line = $lines_ref->[$i];
		chomp $line;
		if($lines_ref->[$i] =~/^-(\S+)\s/) {
			push(@{$item->{name}}, $1);
			$line =~s/^-\S+\s+//;
			push(@{$item->{value}}, $line);
		} elsif($i == $#$lines_ref or $line eq "") {
			return ($item, $i);
		} else {
			$line =~s/^\s+//;
			$item->{value}->[$#{$item->{value}}] .= " $line";
		}
	}
}

sub read_code_block {
	my $lines_ref = shift;
	my $index = shift;
	
	my $code_block;
	for(my $i = $index; $i < scalar(@$lines_ref); $i ++) {
		
		if($lines_ref->[$i] =~/^\s+\S/) {
			for(; $i < scalar(@$lines_ref); $i ++) {
				my $line = $lines_ref->[$i];
				
				
				$line =~s/^\s{2}//;
				if($line eq "") {
					$line = "\n";
				}
				$code_block .= $line;
				if($i == $#$lines_ref or ($lines_ref->[$i + 1] =~/^\s*$/ and $lines_ref->[$i+2] !~/^\s+\S/)) {
					return ($code_block, $i);
				}
			}
		}
	}
}

sub read_paragraph {
	my $lines_ref = shift;
	my $index = shift;
	
	my $paragraph;
	for(my $i = $index; $i < scalar(@$lines_ref); $i ++) {
		my $line = $lines_ref->[$i];
		chomp $line;
		$paragraph .= "$line ";
		if($i == $#$lines_ref or $lines_ref->[$i] eq "") {
			return ($paragraph, $i);
		}
		
	}
}

# read several lines and check whether this is a code chunk
sub is_code_block {
	my $lines_ref = shift;
	my $index = shift;
	
	for(my $i = $index; $i < scalar(@$lines_ref); $i ++) {
	
		if($lines_ref->[$i] =~/^\s+\S/) {
			
			if($i == $#$lines_ref or $lines_ref->[$i + 1] =~/^\s*$/) {
				return 1;
			}
		} else {
			return 0;
		}
	}
	return 0;
}

# get the function name and its arguments
sub get_nearest_function_info {
	my $lines_ref = shift;
	my $index = shift;
	
	my $function_name;
	my $function_args;
	for(my $i = $index; $i < scalar(@$lines_ref); $i ++) {
		my $line = $lines_ref->[$i];
		
		if($line =~/([\w.]+)\s*(=|<-)\s*function\s*\(/) {
			# then find the closing )
            $function_name = $1;
                  
            my $raw_args_str = $POSTMATCH;
            my $left_parenthese_flag = 1; # there are one unmatched left parenthese
            my $closing_position;
            if(($closing_position = find_closing_parenthese($raw_args_str, \$left_parenthese_flag)) > -1) {
                $function_args = substr($raw_args_str, 0, $closing_position);
            } else {
                $function_args = $raw_args_str;
                for($i ++; $i < scalar(@$lines_ref); $i ++) {
					$line = $lines_ref->[$i];
                    chomp $line;
                    $line =~s/^(\s+)//;
                    if(($closing_position = find_closing_parenthese($line, \$left_parenthese_flag)) > -1) {
                        $function_args .= substr($line, 0, $closing_position);
                        last;
                    }
                    $function_args .= " " x (length($function_name)+3) . $line . "\n";
                }
				
				
            }
			$function_args = re_format_function_args($function_args);
			if(my ($g, $c) = check_generic_function($function_name)) {
				return ($function_name, "\\method{$g}{$c}($function_args)");
			} else {
				return ($function_name, "$function_name($function_args)");
			}
		}
	}
	
	return ();
}

sub re_format_function_args {
	my $str = shift;
	my @str = split "\n", $str;
	for(my $i = 0; $i < scalar(@str); $i ++) {
		$str[$i] =~s/^\s+//;
		$str[$i] =~s/\s+$//;
		if($i > 0) {
			$str[$i] = "    $str[$i]";
		}
	}
	return(join "\n", @str);
}


# if find the closing parenthese, return the position in the string
# else return -1
sub find_closing_parenthese {
    my $str = shift;
    my $left_parenthese_flag = shift;
    my @args_char = split "", $str;

    for(my $i = 0; $i < scalar(@args_char); $i ++) {
        if($args_char[$i] eq "(") {
            $$left_parenthese_flag ++;
        }
        elsif($args_char[$i] eq ")") {
            $$left_parenthese_flag --;
        }

        if($$left_parenthese_flag == 0) {
            return $i;
        }
    }
    return -1;
}


# ``arg`` to \code{arg}
# `function` to \code{\link{function}}
# `package::function` to \code{\link[package]{function}}
sub trans_code {
    my $text = shift;
    
    $text =~s/``(.*?)``/\\code{$1}/g;
    
    $text =~s/`(.*?)`/
        my @a = split "::", $1;
        if(scalar(@a) == 2) {
            "\\code{\\link[$a[0]]{$a[1]}}";
        }
        else {
            "\\code{\\link{$a[0]}}";
        }
        /exg;
    return $text;
}

# http://xxx to \url{http:xxx}
sub trans_url {
    my $text = shift;

    $text =~s/(http|ftp|https)(:\/\/\S+)([\s\)\]\}\.,;:\]\)\}]*)/\\url{$1$2}$3/g;

    return $text;
}

sub trans_font {
	
}

sub check_generic_function {
	my $gf = {".__C_BindingFunction" => 1,
			".__C__derivedDefaultMethod" => 1,
			".__C__derivedDefaultMethodWithTrace" => 1,
			".__C__summaryDefault" => 1,
			"aggregate" => 1,
			"all.equal" => 1,
			"anyDuplicated" => 1,
			"aperm" => 1,
			"as.array" => 1,
			"as.character" => 1,
			"as.data.frame" => 1,
			"as.Date" => 1,
			"as.expression" => 1,
			"as.function" => 1,
			"as.list" => 1,
			"as.matrix" => 1,
			"as.null" => 1,
			"as.POSIXct" => 1,
			"as.POSIXlt" => 1,
			"as.single" => 1,
			"as.table" => 1,
			"barplot" => 1,
			"boxplot" => 1,
			"by" => 1,
			"chol" => 1,
			"confint" => 1,
			"contour" => 1,
			"cut" => 1,
			"default.stringsAsFactors" => 1,
			"defaultDumpName" => 1,
			"defaultPrototype" => 1,
			"density" => 1,
			"deriv" => 1,
			"deriv3" => 1,
			"diff" => 1,
			"duplicated" => 1,
			"finalDefaultMethod" => 1,
			"format" => 1,
			"format.summaryDefault" => 1,
			"hist" => 1,
			"image" => 1,
			"is.na<-" => 1,
			"kappa" => 1,
			"labels" => 1,
			"levels" => 1,
			"lines" => 1,
			"mean" => 1,
			"median" => 1,
			"merge" => 1,
			"model.frame" => 1,
			"model.matrix" => 1,
			"pairs" => 1,
			"plot" => 1,
			"points" => 1,
			"pretty" => 1,
			"print" => 1,
			"print.summaryDefault" => 1,
			"qqnorm" => 1,
			"qr" => 1,
			"quantile" => 1,
			"range" => 1,
			"residuals" => 1,
			"rev" => 1,
			"row.names" => 1,
			"row.names<-" => 1,
			"rowsum" => 1,
			"scale" => 1,
			"seq" => 1,
			"showDefault" => 1,
			"solve" => 1,
			"sort" => 1,
			"split" => 1,
			"split<-" => 1,
			"subset" => 1,
			"summary" => 1,
			"t" => 1,
			"terms" => 1,
			"text" => 1,
			"toString" => 1,
			"transform" => 1,
			"unique" => 1,
			"update" => 1,
			"with" => 1,
			"xtfrm" => 1 
		};
	my $f = shift;
	foreach my $key (%$gf) {
		if($f =~/^$key\.(\S+)$/) {
			return ($key, $1);
		}
	}
	return ();
}

# align two vectors
# you can think of it as aligning two DNA sequences
# this is a global alignment
# using dynamic programming
package R::Comment2Man::Align;

# get the score matrix and return the trace-back matrix
sub align_score_matrix {
	my $array1 = shift;
	my $array2 = shift;
	
	my $m = [[]];
	# you can think there is additional letter which is a blank
	# in front of both two vectors and is the start point
	for(my $i = 0; $i <= scalar(@$array1); $i ++) {
		$m->[$i]->[0] = 0;
	}
	for(my $i = 0; $i <= scalar(@$array2); $i ++) {
		$m->[0]->[$i] = 0;
	}
	
	my $direction;
	my $traceback = [[]];
	for(my $i = 1; $i <= scalar(@$array1); $i ++) {
		for(my $j = 1; $j <= scalar(@$array2); $j ++) {
			# calculate the score according to the score rule
			# and record which position the value comes from
			($m->[$i]->[$j], $direction) = align_score($m->[$i]->[$j-1],
			                       $m->[$i-1]->[$j-1],
								   $m->[$i-1]->[$j],
								   $array1->[$i-1] eq $array2->[$j-1]);
			# record the source
			if($direction eq "left") {
				$traceback->[$i]->[$j] = [$i, $j - 1];
			} elsif($direction eq "topleft") {
				$traceback->[$i]->[$j] = [$i - 1, $j - 1];
			} elsif($direction eq "top") {
				$traceback->[$i]->[$j] = [$i - 1, $j];
			}
		}
	}
	return $traceback;
}

# find the maximum value which is coming from
# left, topleft and top positions
sub align_score {
	my $left = shift;
	my $topleft = shift;
	my $top = shift;
	my $is_matched = shift;
	
	my %s = ("matched" => 10,
	         "wrong_matched" => -10,
			 "blank" => 0);
	
	$topleft = $is_matched ? $topleft + $s{matched} : $topleft + $s{wrong_matched};
	$left += $s{blank};
	$top += $s{blank};
	
	if($left >= $topleft and $left >= $top) {
		return($left, "left");
	} elsif($topleft >= $left and $topleft >= $top) {
		return($topleft, "topleft");
	} else {
		return($top, "top");
	}
}

# trace back from bottom right in the matrix
# and finally get the alignment
sub align_traceback {
	my $array1 = shift;
	my $array2 = shift;
	my $traceback = shift;
	
	my $align1 = [];
	my $align2 = [];
	
	my $a = [];
	my $i = scalar(@$array1);
	my $j = scalar(@$array2);
	# this is the trace-back path
	unshift(@$a, [$i, $j]);
	while($i > 0 and $j > 0) {
		($i, $j) = @{$traceback->[$i]->[$j]};
		if(!($i == 0 and $j == 0)) {
			unshift(@$a, [$i, $j]);
		}
	}
	
	# then we can know the alignment
	for(my $i = 0; $i < scalar(@$a); $i ++) {
		if($a->[$i]->[0] == 0) {
			push(@$align1, "");
		} elsif($i > 0 and $a->[$i]->[0] == $a->[$i - 1]->[0]) {
			push(@$align1, "");
		} else {
			push(@$align1, $array1->[$a->[$i]->[0] - 1]);
		}
		
		if($a->[$i]->[1] == 0) {
			push(@$align2, "");
		} elsif($i > 0 and $a->[$i]->[1] == $a->[$i - 1]->[1]) {
			push(@$align2, "");
		} else {
			push(@$align2, $array2->[$a->[$i]->[1] - 1]);
		}
	}
	
	return($align1, $align2);
}

__END__

