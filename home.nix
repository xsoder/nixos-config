{ config, pkgs, ... }:

{
  home.username = "xsoder";
  home.homeDirectory = "/home/xsoder";
  programs.git.enable = true;
  home.stateVersion = "25.05";
  programs.bash = {
    enable = true;
  };
  home.file.".config/i3/config".source = ./dotfiles/i3/config;
  home.file.".config/dunst/dunstrc".source = ./dotfiles/dunst/dunstrc;
  home.file.".vimrc".source = ./dotfiles/vimrc;
  home.file.".xprofile".source = ./dotfiles/xprofile;	

  home.packages = with pkgs; [
    firefox
    neofetch
    feh
    i3status
    emacs
    alacritty
    discord
    btop
    scrot
    dunst
    libnotify
  ];

}
