#!/usr/bin/perl

# Standard Prelude
use strict;
use warnings;
no warnings 'uninitialized';

# imports
use Getopt::Long;
use Data::Dumper;

# list of subtrees we're managing
my $subtrees = {
# I am not using awesome wm anymore
#	AWESOMESOLARIZED => {
#		REPO => 'https://github.com/cycojesus/awesome-solarized.git',
#		PREFIX => 'awesome/.config/awesome/themes/awesome-solarized',
#	},
	LSCOLORS => {
		REPO => 'https://github.com/trapd00r/LS_COLORS.git',
		PREFIX => 'bash/.bash/LS_COLORS',
	},
	CTRLP => {
		PREFIX => 'vim/.vim/bundle/ctrlp.vim',
		REPO => 'https://github.com/ctrlpvim/ctrlp.vim.git',
	},
	VIMPATHOGEN => {
		PREFIX => 'vim/.vim/bundle/vim-pathogen',
		REPO => 'https://github.com/tpope/vim-pathogen.git',
	},
	VIMSLEUTH => {
		PREFIX => 'vim/.vim/bundle/vim-sleuth',
		REPO => 'https://github.com/tpope/vim-sleuth.git',
	},
	VIMSURROUND => {
		PREFIX => 'vim/.vim/bundle/vim-surround',
		REPO => 'https://github.com/tpope/vim-surround.git',
	},
	ANTIGENHS => {
		PREFIX => 'zsh/.zsh/antigen-hs',
		REPO => 'https://github.com/Tarrasch/antigen-hs.git',
	},
	ORGOXBBCODE => {
		PREFIX => 'emacs/.emacs.d/libs/org-ox-bbcode',
		REPO => 'https://github.com/levindu/org-ox-bbcode.git',
	},
};

# things we can do with them

sub DoSubtree {
	my ($args) = @_;
	my $tree = $args->{SUBTREE};
	my $cmd = $args->{CMD};
	print(qx{git subtree $cmd --prefix $tree->{PREFIX} $tree->{REPO} master --squash});
}

sub DoSubtrees {
	my ($args) = @_;
	for my $treename (keys $args->{SUBTREES}) {
		DoSubtree({
			SUBTREE => $args->{SUBTREES}->{$treename},
			CMD => $args->{CMD},
		});
	}
}

# argument handling
sub ParseSubtreeList {
	my ($args) = @_;
	my $optarg = $args->{OPTARG};
	my $trees;
	if (!$optarg) {
		$trees = $subtrees;
	}
	else {
		my @treenames = grep { $_ ne '' } split(/[,\s]+/, $optarg);
		$trees = {map { $_ => $subtrees->{$_} } @treenames};
	}
	return $trees;
}

sub _Main {
	GetOptions(
		'add:s' => \my $opt_add,
		'pull:s' => \my $opt_pull,
		'list' => \my $opt_list,
	);

	if (defined $opt_list) {
		print(Dumper($subtrees));
	}
	elsif (defined $opt_add) {
		DoSubtrees({
			SUBTREES => ParseSubtreeList({OPTARG => $opt_add}),
			CMD => 'add',
		});
	}
	elsif (defined $opt_pull) {
		DoSubtrees({
			SUBTREES => ParseSubtreeList({OPTARG => $opt_pull}),
			CMD => 'pull',
		});
	}
}

_Main();
