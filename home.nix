{ config, pkgs, ... }:

{
  home.username = "xsoder";
  home.homeDirectory = "/home/xsoder";
  programs.git.enable = true;
  home.stateVersion = "25.05";
  programs.bash = {
    enable = true;
  };
  home.file = {
    ".config/i3/config".source = ./dotfiles/i3/config;
    ".config/dunst/dunstrc".source = ./dotfiles/dunst/dunstrc;
    ".vimrc".source = ./dotfiles/vimrc;
    ".vim/colors.vim".source = ./dotfiles/vim/colors.vim;
    ".vim/fzf.vim".source = ./dotfiles/vim/fzf.vim;
    ".vim/keybinds.vim".source = ./dotfiles/vim/keybinds.vim;
    ".vim/options.vim".source = ./dotfiles/vim/options.vim;
    ".vim/plugins.vim".source = ./dotfiles/vim/plugins.vim;
    ".xprofile".source = ./dotfiles/xprofile;	
  };


  home.packages = with pkgs; [
    firefox
    neofetch
    feh
    i3status
    emacs
    alacritty
    fzf
    ripgrep
    fd
    discord
    btop
    scrot
    dunst
    libnotify
  ];

}
