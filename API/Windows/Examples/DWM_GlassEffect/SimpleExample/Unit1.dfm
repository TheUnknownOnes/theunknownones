object Form1: TForm1
  Left = 214
  Top = 116
  Caption = 'Form1'
  ClientHeight = 280
  ClientWidth = 291
  Color = 15790320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
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
  object Button3: TButton
    Left = 16
    Top = 39
    Width = 120
    Height = 25
    Caption = 'extend Frame to client'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 16
    Top = 240
    Width = 120
    Height = 25
    Caption = 'active Colorization Color'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 16
    Top = 101
    Width = 120
    Height = 25
    Caption = 'Region Blur'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Edit1: TEdit
    Left = 112
    Top = 152
    Width = 73
    Height = 21
    TabOrder = 5
    Text = '10'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 112
    Top = 200
    Width = 73
    Height = 21
    TabOrder = 6
    Text = '10'
    OnChange = Edit1Change
  end
  object Edit3: TEdit
    Left = 33
    Top = 176
    Width = 73
    Height = 21
    TabOrder = 7
    Text = '10'
    OnChange = Edit1Change
  end
  object Edit4: TEdit
    Left = 191
    Top = 173
    Width = 73
    Height = 21
    TabOrder = 8
    Text = '10'
    OnChange = Edit1Change
  end
end
