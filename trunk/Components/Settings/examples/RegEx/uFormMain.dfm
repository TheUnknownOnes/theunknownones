object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'RegEx'
  ClientHeight = 133
  ClientWidth = 356
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
    Width = 337
    Height = 89
    ItemHeight = 13
    TabOrder = 0
  end
  object btn_ListUsersForm1Widths: TButton
    Left = 8
    Top = 103
    Width = 169
    Height = 25
    Caption = 'List Users Form1 Widths'
    TabOrder = 1
    OnClick = btn_ListUsersForm1WidthsClick
  end
  object Settings1: TSettingsFile
    Left = 248
    Top = 56
  end
end
