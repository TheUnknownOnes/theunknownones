object Form1: TForm1
  Left = 0
  Top = 0
  Width = 116
  Height = 165
  Caption = 'LED'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btn_Button2: TButton
    Left = 16
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 0
    OnClick = btn_Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Scroll Lock LED'
    TabOrder = 1
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 24
    Width = 97
    Height = 17
    Caption = 'CAPS Lock LED'
    TabOrder = 2
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 40
    Width = 97
    Height = 17
    Caption = 'NUM LockLED'
    TabOrder = 3
  end
  object Reset: TButton
    Left = 16
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 4
    OnClick = ResetClick
  end
end
