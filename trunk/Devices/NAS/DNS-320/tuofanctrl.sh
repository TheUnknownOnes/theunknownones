#!/ffp/bin/sh
PATH=/ffp/bin:/ffp/sbin:$PATH

# fan-control for DNS-320 with fun_plug 0.7
# Developed by TheUnknownOnes.net
# This script doesnt work with a cron-job.
# Therefore it should be rock solid. ;)

# config

# if you want to see some really beautifull :) log lines, set logfile to a valid name
# default: (empty)
logfile=
#logfile="/ffp/var/log/tuofanctrl.log"

# checkinterval specifies the timespan between two checks of the temperature
# internally its used as parameter for the sleep command
# because of that you can specify "s", "m", "h" or "d" as suffix for seconds, minutes, hours or days
# default: 5m
checkinterval=5m

# Temperatures (all specified in Celsius)
# shutdown_over specifies over which temperature the system should shutdown
# default: 60
shutdown_over=60

# low_speed_under specifies under which temperature the fan-speed should be set to low
# default: 40
low_speed_under=40

# stop_under specifies under which temperature the fan should be switched off
# default: 30
stop_under=30

# with check_low_speed set to 1, this script will check wether the fan is rotating after setting it to low
# if it is not rotating, the speed will be set to high
# this is usefull, if you use a 12V-fan which only rotates if the voltage is high enough respectivly the speed is set to high
# if you need either high-speed of stopped fan, you should set stop_under and low_speed_under to the same value
# default: 1
check_low_speed=1

# set only_rotate_if_hdd_active to 1 if you have hdparm installed and 
# you want the fan only to be active if at least one harddisk is active
# default: 0
only_rotate_if_hdd_active=0


# PROVIDE: tuofanctrl
# REQUIRE: LOGIN

. /ffp/etc/ffp.subr

name="tuofanctrl"
extra_commands="daemon checknow"
start_cmd="rc_start"
stop_cmd="rc_stop"
daemon_cmd="rc_daemon"
checknow_cmd="rc_checknow"

pidfile=/ffp/var/run/tuofanctrl.pid


log() 
{
  if [ -n "$logfile" ] 
  then
    dt=$(date "+%d.%m.%y %H:%M:%S")   
    echo "[$dt] $@" >> $logfile
  fi  
}


rc_start() 
{  
  pid=$(cat $pidfile)
  if [ -d /proc/$pid ] 
  then
    echo "$name is already running"
    exit
  fi
  
  echo -n "Starting $name ... "
    
  # kill the original fan-controller 
  fc_pid=$(pidof fan_control)
  [ -n "$fc_pid" ] && kill -9 $fc_pid >/dev/null 2>&1
  
  # start this script in daemon mode   
  $0 daemon & >/dev/null 2>&1  
  if [ $? -eq 0 ] 
  then
    echo "ok"
  else
    echo "failed"
  fi  
}

rc_stop() 
{
  echo -n "Stopping $name ... "
  pid=$(cat $pidfile)  
  [ -n $pid ] && kill -9 $pid >/dev/null 2>&1 && echo "ok"
  [ $? -ne 0 ] && echo "failed"
  
  # Start the original fan-controller
  fan_control 0 c & >/dev/null 2>&1 
}

rc_daemon() 
{
  echo $$ > $pidfile  
  while [ true ] 
  do
    $0 checknow >/dev/null 2>&1
    sleep $checkinterval
  done
}

rc_checknow() 
{ 
  temp=$(FT_testing -T | grep "Temperature is" | cut -d " " -f 3)
  log "Current temperature is $temp°C"
  
  if [ $only_rotate_if_hdd_active -gt 0 -a -x /ffp/sbin/hdparm ]
  then
    disks_active=($(hdparm -C /dev/sda | grep -i -c "active") + $(hdparm -C /dev/sdb | grep -i -c "active"))
    if [ $disks_active -eq 0 ] 
    then
      log "No disks active. Fan will be switched off"
      FT_testing -S >/dev/null 2>&1
      exit
    fi  
  fi
  
  # default fan-speed is full-speed
  new_fanspeed="-F"
  
  # if the temperature is above "shutdown_over" the system will be shutdown
  if [ $temp -gt $shutdown_over ]
  then
    new_fanspeed="-P"
  fi
  
  # if the temperature is under "low_speed_under" the fan-speed is set to low
  if [ $temp -lt $low_speed_under ]
  then    
    new_fanspeed="-L"  
  fi
  
  # if the temperature is under "stop_under" the fan-speed is set to stop
  if [ $temp -lt $stop_under ]
  then    
    new_fanspeed="-S"
  fi
  
  log "$(FT_testing $new_fanspeed)"
  
  # maybe we should check wether the fan is rotating
  if [ $check_low_speed -eq 1 -a "$new_fanspeed" = "-L" ] 
  then   
    sleep 5s
    state=$(FT_testing -G | tail -1)
    
    if [ "$state" = "stop" ] 
    then
      log "Fan is not rotating with low speed. It will be set to full speed."
      log "$(FT_testing -F)"
    fi
  fi
}

run_rc_command "$1"
