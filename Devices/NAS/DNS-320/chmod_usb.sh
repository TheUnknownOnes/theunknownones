#!/ffp/bin/sh

# log everything to a file
exec >>/usr/local/config/chmod.log 2>&1

args="$@"
NOW=$(date +"%Y-%m-%d %T")

#these are the chmod args we want to suppress
suppress_args="777 -R /mnt/USB/HD.*"

if [[ ! "$args" =~ $suppress_args ]]; then
    echo "[$NOW] Executing chmod $args"
    /bin/busybox chmod $@
else
   echo "[$NOW] Suppressing chmod on /mnt/USB/HD*"
fi

