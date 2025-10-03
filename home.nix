{ config, pkgs, ... }:
let
  dotDir = name: path: {
    ${name} = {
      source = path;
      recursive = true;
    };
  };
in
{
  home.username = "xsoder";
  home.homeDirectory = "/home/xsoder";
  home.stateVersion = "25.05";
  programs.git.enable = true;
  programs.bash.enable = true;
  programs.starship.enable = true;
  home.file =
    dotDir ".emacs.local" ./dotfiles/emacs/emacs.local
    // dotDir ".emacs.rc" ./dotfiles/emacs/emacs.rc
    // {
      ".config/i3/config".source = ./dotfiles/i3/config;
      ".config/alacritty/alacritty.toml".source = ./dotfiles/alacritty/alacritty.toml;
      ".config/dunst/dunstrc".source = ./dotfiles/dunst/dunstrc;
      ".vimrc".source = ./dotfiles/vimrc;
      ".emacs".source = ./dotfiles/emacs/emacs;
      ".emacs.custom".source = ./dotfiles/emacs/emacs.custom.el;
      ".vim/colors.vim".source = ./dotfiles/vim/colors.vim;
      ".vim/comp.vim".source = ./dotfiles/vim/comp.vim;
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
    tmux
    starship
  ];
}
