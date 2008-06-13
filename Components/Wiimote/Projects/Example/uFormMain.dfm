object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 292
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 521
    Height = 89
    ItemHeight = 13
    TabOrder = 0
  end
  object btn_Connnect: TButton
    Left = 454
    Top = 103
    Width = 75
    Height = 25
    Caption = 'Connnect'
    TabOrder = 1
    OnClick = btn_ConnnectClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 103
    Width = 369
    Height = 181
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 88
    Top = 128
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer2Timer
    Left = 192
    Top = 208
  end
end
