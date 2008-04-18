object form_Main: Tform_Main
  Left = 0
  Top = 0
  Caption = 'rss2disk'
  ClientHeight = 301
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gb_Feeds: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 639
    Height = 158
    Align = alTop
    Caption = 'Feeds'
    TabOrder = 0
    ExplicitWidth = 556
    object lv_Feeds: TListView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 629
      Height = 135
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 100
        end
        item
          Caption = 'Source'
          Width = 150
        end
        item
          Caption = 'Save to'
          Width = 200
        end
        item
          Caption = 'State'
          Width = 150
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      ExplicitLeft = 192
      ExplicitTop = 8
      ExplicitWidth = 250
      ExplicitHeight = 150
    end
  end
  object MainMenu: TMainMenu
    Left = 288
    Top = 184
    object Feeds1: TMenuItem
      Caption = 'Feeds'
      object mi_Addfeed: TMenuItem
        Caption = 'Add feed ...'
        ShortCut = 16462
        OnClick = mi_AddfeedClick
      end
      object mi_Modifyfeed: TMenuItem
        Caption = 'Modify feed ...'
      end
      object mi_Deletefeed: TMenuItem
        Caption = 'Delete feed'
        ShortCut = 46
      end
    end
  end
end
