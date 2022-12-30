```
sh <(curl -L https://nixos.org/nix/install) --daemon
```

I will:

 - make sure your computer doesn't already have Nix files
   (if it does, I will tell you how to clean them up.)
 - create local users (see the list above for the users I'll make)
 - create a local group (nixbld)
 
 - install Nix in to `/nix`
 
 - create a configuration file in `/etc/nix`

 - set up the `"default profile"` by creating some Nix-related files in
   `/root`
   
 - back up `/etc/bashrc` to `/etc/bashrc.backup-before-nix`
 - update `/etc/bashrc` to include some Nix configuration
 
 - back up `/etc/zshrc` to `/etc/zshrc.backup-before-nix`
 - update `/etc/zshrc` to include some Nix configuration
 
 - back up `/etc/bash.bashrc` to `/etc/bash.bashrc.backup-before-nix`
 - update `/etc/bash.bashrc` to include some Nix configuration

 - load and start a service (at `/etc/systemd/system/nix-daemon.service`
   and `/etc/systemd/system/nix-daemon.socket`) for nix-daemon


---- Nix config report ---------------------------------------------------------
        Temp Dir:	/tmp/tmp.GvZyOjhPn5
        Nix Root:	/nix
     Build Users:	32
  Build Group ID:	30000
Build Group Name:	nixbld

build users:
    Username:	UID
     nixbld1:	30001
     nixbld2:	30002
     nixbld3:	30003
     nixbld4:	30004
     nixbld5:	30005
     nixbld6:	30006
     nixbld7:	30007
     nixbld8:	30008
     nixbld9:	30009
     nixbld10:	30010
     nixbld11:	30011
     nixbld12:	30012
     nixbld13:	30013
     nixbld14:	30014
     nixbld15:	30015
     nixbld16:	30016
     nixbld17:	30017
     nixbld18:	30018
     nixbld19:	30019
     nixbld20:	30020
     nixbld21:	30021
     nixbld22:	30022
     nixbld23:	30023
     nixbld24:	30024
     nixbld25:	30025
     nixbld26:	30026
     nixbld27:	30027
     nixbld28:	30028
     nixbld29:	30029
     nixbld30:	30030
     nixbld31:	30031
     nixbld32:	30032




~> Setting up the build group nixbld
            Exists:	Yes

~> Setting up the build user nixbld1
            Exists:	Yes
            Hidden:	Yes
    Home Directory:	/var/empty



**Lots of stuff not recorded**

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo systemctl restart nix-daemon.service

to start the nix-daemon.service

Alright! We're done!
Try it! Open a new terminal, and type:

  $ nix-shell -p nix-info --run "nix-info -m"

Thank you for using this installer. If you have any feedback or need
help, don't hesitate:

You can open an issue at https://github.com/nixos/nix/issues

Or feel free to contact the team:
 - Matrix: #nix:nixos.org
 - IRC: in #nixos on irc.libera.chat
 - twitter: @nixos_org
 - forum: https://discourse.nixos.org
