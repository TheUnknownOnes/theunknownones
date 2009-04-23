object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 312
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 193
    Height = 89
    ItemHeight = 13
    TabOrder = 0
  end
  object btn_Connnect: TButton
    Left = 227
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Connnect'
    TabOrder = 1
    OnClick = btn_ConnnectClick
  end
  object pb_Battery: TProgressBar
    Left = 207
    Top = 8
    Width = 14
    Height = 89
    Orientation = pbVertical
    TabOrder = 2
  end
  object pb_IRPoints: TProgressBar
    Left = 227
    Top = 81
    Width = 75
    Height = 16
    Max = 4
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 24
    Top = 132
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 24
    Top = 158
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'Edit1'
  end
  object Edit3: TEdit
    Left = 24
    Top = 185
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
  end
  object Wiimote1: TWiimote
    OnNewReport = Wiimote1NewReport
    OnStatus = Wiimote1Status
    OnConnected = Wiimote1Connected
    OnDisconnected = Wiimote1Disconnected
    OnButtonDown = Wiimote1ButtonDown
    OnButtonUp = Wiimote1ButtonUp
    OnButtonIsDown = Wiimote1ButtonIsDown
    Left = 192
    Top = 152
  end
  object tm_Battery: TTimer
    OnTimer = tm_BatteryTimer
    Left = 448
    Top = 40
  end
  object tm_Leds: TTimer
    Interval = 5000
    OnTimer = tm_LedsTimer
    Left = 456
    Top = 72
  end
end
