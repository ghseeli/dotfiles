Notes on setting up an OS X system
===

1. Change capslock to control

In Big Sur, this can be done under Settings -> Keyboard -> Modifier Keys 

1. Install XCode and XCode developer tools

XCode is available in the app store, some developer tools will be installed automatically by trying to run `git`, and the rest can be done via `xcode-select --install`.

1. Clone these dotfiles

Note that OS X now uses zsh as the default shell

1. Allow installation from anywhere

For BigSur, run `sudo spctl --master-disable` to disable the security feature. One can always change it back in the system preferences.

1. Install [oh-my-zsh](https://ohmyz.sh/#install).

1. Install [homebrew]().
