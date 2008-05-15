{*
 *	wiiuse
 *
 *	Written By:
 *		Michael Laforest	< para >
 *		Email: < thepara (--AT--) g m a i l [--DOT--] com >
 *
 *	Copyright 2006-2007
 *
 *	This file is part of wiiuse.
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *}

unit uWiiuse;

interface

uses
  Classes,
  Windows;

const
  LibraryName = 'wiiuse.dll';

const
  { led bit masks }
  WIIMOTE_LED_NONE			 = $00;
  WIIMOTE_LED_1					 = $10;
  WIIMOTE_LED_2					 = $20;
  WIIMOTE_LED_3					 = $40;
  WIIMOTE_LED_4					 = $80;

  { button codes }
  WIIMOTE_BUTTON_TWO				 = $0001;
  WIIMOTE_BUTTON_ONE				 = $0002;
  WIIMOTE_BUTTON_B				 = $0004;
  WIIMOTE_BUTTON_A				 = $0008;
  WIIMOTE_BUTTON_MINUS			 = $0010;
  WIIMOTE_BUTTON_ZACCEL_BIT6		 = $0020;
  WIIMOTE_BUTTON_ZACCEL_BIT7		 = $0040;
  WIIMOTE_BUTTON_HOME				 = $0080;
  WIIMOTE_BUTTON_LEFT				 = $0100;
  WIIMOTE_BUTTON_RIGHT			 = $0200;
  WIIMOTE_BUTTON_DOWN				 = $0400;
  WIIMOTE_BUTTON_UP				 = $0800;
  WIIMOTE_BUTTON_PLUS				 = $1000;
  WIIMOTE_BUTTON_ZACCEL_BIT4		 = $2000;
  WIIMOTE_BUTTON_ZACCEL_BIT5		 = $4000;
  WIIMOTE_BUTTON_UNKNOWN			 = $8000;
  WIIMOTE_BUTTON_ALL				 = $1F9F;

  { nunchul button codes }
  NUNCHUK_BUTTON_Z				 = $01;
  NUNCHUK_BUTTON_C				 = $02;
  NUNCHUK_BUTTON_ALL				 = $03;

  { classic controller button codes }
  CLASSIC_CTRL_BUTTON_UP			 = $0001;
  CLASSIC_CTRL_BUTTON_LEFT		 = $0002;
  CLASSIC_CTRL_BUTTON_ZR			 = $0004;
  CLASSIC_CTRL_BUTTON_X			 = $0008;
  CLASSIC_CTRL_BUTTON_A			 = $0010;
  CLASSIC_CTRL_BUTTON_Y			 = $0020;
  CLASSIC_CTRL_BUTTON_B			 = $0040;
  CLASSIC_CTRL_BUTTON_ZL			 = $0080;
  CLASSIC_CTRL_BUTTON_FULL_R		 = $0200;
  CLASSIC_CTRL_BUTTON_PLUS		 = $0400;
  CLASSIC_CTRL_BUTTON_HOME		 = $0800;
  CLASSIC_CTRL_BUTTON_MINUS		 = $1000;
  CLASSIC_CTRL_BUTTON_FULL_L		 = $2000;
  CLASSIC_CTRL_BUTTON_DOWN		 = $4000;
  CLASSIC_CTRL_BUTTON_RIGHT		 = $8000;
  CLASSIC_CTRL_BUTTON_ALL			 = $FEFF;

  { guitar hero 3 button codes }
  GUITAR_HERO_3_BUTTON_STRUM_UP	 = $0001;
  GUITAR_HERO_3_BUTTON_YELLOW		 = $0008;
  GUITAR_HERO_3_BUTTON_GREEN		 = $0010;
  GUITAR_HERO_3_BUTTON_BLUE		 = $0020;
  GUITAR_HERO_3_BUTTON_RED		 = $0040;
  GUITAR_HERO_3_BUTTON_ORANGE		 = $0080;
  GUITAR_HERO_3_BUTTON_PLUS		 = $0400;
  GUITAR_HERO_3_BUTTON_MINUS		 = $1000;
  GUITAR_HERO_3_BUTTON_STRUM_DOWN	 = $4000;
  GUITAR_HERO_3_BUTTON_ALL		 = $FEFF;


  { wiimote option flags }
  WIIUSE_SMOOTHING				 = $01;
  WIIUSE_CONTINUOUS				 = $02;
  WIIUSE_ORIENT_THRESH			 = $04;
  WIIUSE_INIT_FLAGS	 = (WIIUSE_SMOOTHING or WIIUSE_ORIENT_THRESH);

  WIIUSE_ORIENT_PRECISION			 = 100.0;

  { expansion codes }
  EXP_NONE						 = 0;
  EXP_NUNCHUK					 = 1;
  EXP_CLASSIC					 = 2;
  EXP_GUITAR_HERO_3		 = 3;

  MAX_PAYLOAD = 32;

type
  ir_position_t = (WIIUSE_IR_ABOVE,
                   WIIUSE_IR_BELOW,
                   WIIUSE_FOR_POSITION_SIZE = $FFFFFF);

  vec2b_t = record
    x, y : Byte;
  end;

  vec3b_t = record
    x, y, z : Byte;
  end;

  vec3f_t = record
    x, y, z : Single;
  end;

  orient_t = record
    roll,               //roll, this may be smoothed if enabled
    pitch,              //pitch, this may be smoothed if enabled
    yaw : Single;

    a_roll,             //absolute roll, unsmoothed
    a_pitch : Single;   //absolute pitch, unsmoothed
  end;

  gforce_t = record
    x, y, z : Single;
  end;

  accel_t = record
    cal_zero,          //zero calibration
    cal_g : vec3b_t;   //1g difference around 0cal

    st_roll,           //last smoothed roll value
    st_pitch,          //last smoothed roll pitch
    st_alpha : Single; //alpha value for smoothing [0-1]
  end;

  ir_dot_t = record
    visible : Byte;    //if the IR source is visible

    x,                 //interpolated X coordinate
    y : UINT;          //interpolated Y coordinate

    rx,                //raw X coordinate (0-1023)
    ry : SHORT;        //raw Y coordinate (0-767)

    order : Byte;      //increasing order by x-axis value

    size : Byte;       //size of the IR dot (0-15)
  end;

  aspect_t = (WIIUSE_ASPECT_4_3,
              WIIUSE_ASPECT_16_9,
              WIIUSE_FOR_ASPECT_SIZE = $FFFFFF);

  ir_t = record
    dot : array[0..3] of ir_dot_t;   //IR dots
    num_dots : Byte;                 //number of dots at this time

    aspect : aspect_t;               //aspect ratio of the screen

    pos : ir_position_t;             //IR sensor bar position

    vres : array[0..1] of UINT;      //IR virtual screen resolution
    offset : array[0..1] of Integer; //IR XY correction offset
    state : Integer;                 //keeps track of the IR state

    ax,                              //absolute X coordinate
    ay : Integer;                    //absolute Y coordinate

    x,                               //calculated X coordinate
    y : Integer;                     //calculated Y coordinate

    distance : Single;               //pixel distance between first 2 dots
    z : Single;                      //calculated distance
  end;


  {*
  *	The angle \a ang is relative to the positive y-axis into quadrant I
  *	and ranges from 0 to 360 degrees.  So if the joystick is held straight
  *	upwards then angle is 0 degrees.  If it is held to the right it is 90,
  *	down is 180, and left is 270.
  *
  *	The magnitude \a mag is the distance from the center to where the
  *	joystick is being held.  The magnitude ranges from 0 to 1.
  *	If the joystick is only slightly tilted from the center the magnitude
  *	will be low, but if it is closer to the outter edge the value will
  *	be higher.
  *}
  joystick_t = record
    max,               //maximum joystick values
    min,               //minimum joystick values
    center : vec2b_t;  //center joystick values

    ang,               //angle the joystick is being held
    mag : Single;      //magnitude of the joystick (range 0-1)
  end;

  nunchuk_t = record
    accel_calib : accel_t;      //nunchuk accelerometer calibration
    js : joystick_t;            //joystick calibration

    flags : PInteger;           //options flag (points to wiimote_t.flags)

    btns,                       //what buttons have just been pressed
    btns_held,                  //what buttons are being held down
    btns_released : Byte;       //what buttons were just released this

    orient_threshold : Single;  //threshold for orient to generate an event
    accel_threshold : Integer;  //threshold for accel to generate an event

    accel : vec3b_t;            //current raw acceleration data
    orient : orient_t;          //current orientation on each axis
    gforce : gforce_t;          //current gravity forces on each axis
  end;

  classic_ctrl_t = record
    btns,                   //what buttons have just been pressed
    btns_held,              //what buttons are being held down
    btns_released : SHORT;  //what buttons were just released this

    r_shoulder,             //right shoulder button (range 0-1)
    l_shoulder : Single;    //left shoulder button (range 0-1)

    ljs,                    //left joystick calibration
    rjs : joystick_t        //right joystick calibration
  end;

  guitar_hero_3_t = record
    btns,                   //what buttons have just been pressed
    btns_held,              //what buttons are being held down
    btns_released : SHORT;  //what buttons were just released this

    whammy_bar : Single;    //whammy bar (range 0-1)

    js : joystick_t;        //joystick calibration
  end;

  expansion_t = record
    case _type : Integer of
      1: (nunchuck : nunchuk_t);
      2: (classic : classic_ctrl_t);
      3: (gh3 :guitar_hero_3_t);
  end;

  win_bt_stack_t = (WIIUSE_STACK_UNKNOWN,
                    WIIUSE_STACK_MS,
                    WIIUSE_STACK_BLUESOLEIL,
                    WIIUSE_FOR_STACK_SIZE = $FFFFFF);

  wiimote_state_t = record
    exp_ljs_ang,
    exp_rjs_ang,
    exp_ljs_mag,
    exp_rjs_mag : Single;
    exp_btns : Word;
    exp_orient : orient_t;
    exp_accel : vec3b_t;
    exp_r_shoulder,
    exp_l_shoulder : Single;

    //ir_t
    ir_ax,
    ir_ay : Integer;
    ir_distance : Single;

    orient : orient_t;
    btns : Word;

    accel : vec3b_t;
  end;

  WIIUSE_EVENT_TYPE = ( WIIUSE_NONE = 0,
                        WIIUSE_EVENT,
                        WIIUSE_STATUS,
                        WIIUSE_CONNECT,
                        WIIUSE_DISCONNECT,
                        WIIUSE_UNEXPECTED_DISCONNECT,
                        WIIUSE_READ_DATA,
                        WIIUSE_NUNCHUK_INSERTED,
                        WIIUSE_NUNCHUK_REMOVED,
                        WIIUSE_CLASSIC_CTRL_INSERTED,
                        WIIUSE_CLASSIC_CTRL_REMOVED,
                        WIIUSE_GUITAR_HERO_3_CTRL_INSERTED,
                        WIIUSE_GUITAR_HERO_3_CTRL_REMOVED,
                        WIIUSE_FOR_EVENT_SIZE = $FFFFFF);

  Pwiimote_t = ^wiimote_t;

  {**
   *      A registered function of this type is called automatically by the wiiuse
   *      library when the wiimote has returned the full data requested by a previous
   *      call to wiiuse_read_data().
   *}
  wiiuse_read_cb = procedure(wm : Pwiimote_t; data : PByte; len : Word); stdcall;


  Pread_req_t = ^read_req_t;
  read_req_t = record
    cb : wiiuse_read_cb;  //read data callback
    buf : PByte;          //buffer where read data is written
    addr,                 //the offset that the read started at
    size,                 //the length of the data read
    wait : UINT;          //num bytes still needed to finish read
    dirty : Byte;         //set to 1 if not using callback and needs to be cleaned up

    next : Pread_req_t;
  end;

  wiimote_t = record
    unid : Integer;                    //user specified id

    dev_handle : THandle;              //HID handle
    hid_overlap : OVERLAPPED;          //overlap handle
    stack : win_bt_stack_t;            //type of bluetooth stack to use
    timeout : Integer;                 //read timeout
    normal_timeout : Byte;             //normal timeout
    exp_timeout : Byte;                //timeout for expansion handshake

    state : Integer;                   //various state flags
    leds : Byte;                       //currently lit leds
    battery_level : Single;            //battery level

    flags : Integer;                   //options flag

    handshake_state : Byte;            //the state of the connection handshake

    read_req : Pread_req_t;            //list of data read requests
    accel_calib : accel_t;             //wiimote accelerometer calibration
    exp : expansion_t;                 //wiimote expansion device

    accel : vec3b_t;                   //current raw acceleration data
    orient : orient_t;                 //current orientation on each axis
    gforce : gforce_t;                 //current gravity forces on each axis

    ir : ir_t;                         //IR data

    btns,                              //what buttons have just been pressed
    btns_held,                         //what buttons are being held down
    btns_released : Word;              //what buttons were just released this

    orient_threshold : Single;         //threshold for orient to generate an event
    accel_threshold : Integer;         //threshold for accel to generate an event

    lstate : wiimote_state_t;          //last saved state

    event : WIIUSE_EVENT_TYPE;         //type of event that occured
    event_buf : array[0..MAX_PAYLOAD-1] of Byte; //event buffer
  end;

  TWiimotes = array of wiimote_t;
  PWiimotes = ^TWiimotes;

  function wiiuse_version() : PChar; cdecl; external LibraryName;
  function wiiuse_init(wiimotes : Integer) : PWiimotes; cdecl; external LibraryName;
  procedure wiiuse_disconnected(wm : Pwiimote_t); cdecl; external LibraryName;
  procedure wiiuse_cleanup(wm : PWiimotes; wiimotes : Integer); cdecl; external LibraryName;
  procedure wiiuse_rumble(wm : Pwiimote_t; status : Integer); cdecl; external LibraryName;
  procedure wiiuse_toggle_rumble(wm : Pwiimote_t); cdecl; external LibraryName;
  procedure wiiuse_set_leds(wm : Pwiimote_t; leds : Integer); cdecl; external LibraryName;
  procedure wiiuse_motion_sensing(wm : Pwiimote_t; status : Integer); cdecl; external LibraryName;
  function wiiuse_read_data_(wm : Pwiimote_t; buffer : PByte; offset : UINT; len : Word) : Integer; cdecl; external LibraryName name 'wiiuse_read_data';
  function wiiuse_write_data(wm : Pwiimote_t; adr : UINT; data : PByte; len : Byte) : Integer; cdecl; external LibraryName;
  procedure wiiuse_status_(wm : Pwiimote_t); cdecl; external LibraryName name 'wiiuse_status';
  function wiiuse_get_by_id(wm : PWiimotes; wiimotes : Integer; unid : Integer) : Pwiimote_t; cdecl; external LibraryName;
  function wiiuse_set_flags(wm : Pwiimote_t; enable : Integer; disable : Integer) : Integer; cdecl; external LibraryName;
  function wiiuse_set_smooth_alpha(wm : Pwiimote_t; alpha : Single) : Single; cdecl; external LibraryName;
  procedure wiiuse_set_bluetooth_stack(wm : PWiimotes; wiimotes : Integer; type_ : win_bt_stack_t); cdecl; external LibraryName;
  procedure wiiuse_set_orient_threshold(wm : Pwiimote_t; threshold : single); cdecl; external LibraryName;
  procedure wiiuse_resync(wm : Pwiimote_t); cdecl; external LibraryName;
  procedure wiiuse_set_timeout(wm : PWiimotes; wiimotes : Integer; normal_timeout : Byte; exp_timeout : Byte); cdecl; external LibraryName;
  procedure wiiuse_set_accel_threshold(wm : Pwiimote_t; threshold : Integer); cdecl; external LibraryName;

  function wiiuse_find(wm : PWiimotes; max_wiimotes : Integer; timeout : Integer) : Integer; cdecl; external LibraryName;
  function wiiuse_connect_(wm : PWiimotes; max_wiimotes : Integer) : Integer; cdecl; external LibraryName name 'wiiuse_connect';
  procedure wiiuse_disconnect_(wm : Pwiimote_t); cdecl; external LibraryName name 'wiiuse_disconnect';

  function wiiuse_poll(wm : PWiimotes; wiimotes : Integer) : Integer; cdecl; external LibraryName;

  procedure wiiuse_set_ir(wm : Pwiimote_t; status : Integer); cdecl; external LibraryName;
  procedure wiiuse_set_ir_vres(wm : Pwiimote_t; x, y : UINT); cdecl; external LibraryName;
  procedure wiiuse_set_ir_position(wm : Pwiimote_t; pos : ir_position_t); cdecl; external LibraryName;
  procedure wiiuse_set_aspect_ratio(wm : Pwiimote_t; aspect : aspect_t); cdecl; external LibraryName;
  procedure wiiuse_set_ir_sensitivity(wm : Pwiimote_t; level : Integer); cdecl; external LibraryName;

  procedure wiiuse_set_nunchuk_orient_threshold(wm : Pwiimote_t; threshold : Single); cdecl; external LibraryName;
  procedure wiiuse_set_nunchuk_accel_threshold(wm : Pwiimote_t; threshold : Integer); cdecl; external LibraryName;


implementation


end.
