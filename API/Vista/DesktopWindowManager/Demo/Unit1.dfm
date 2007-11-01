object Form1: TForm1
  Left = 214
  Top = 116
  Caption = 'Form1'
  ClientHeight = 246
  ClientWidth = 269
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Blur behind'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 1
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 70
    Width = 75
    Height = 25
    Caption = 'extend Frame'
    TabOrder = 1
    OnClick = Button2Click
  end
  object SpinEdit1: TSpinEdit
    Left = 119
    Top = 103
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit2: TSpinEdit
    Left = 119
    Top = 159
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit3: TSpinEdit
    Left = 48
    Top = 131
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit4: TSpinEdit
    Left = 190
    Top = 131
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
    OnChange = SpinEdit1Change
  end
  object Button3: TButton
    Left = 16
    Top = 39
    Width = 120
    Height = 25
    Caption = 'extend Frame to client'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 16
    Top = 192
    Width = 120
    Height = 25
    Caption = 'active Colorization Color'
    TabOrder = 7
    OnClick = Button4Click
  end
end
