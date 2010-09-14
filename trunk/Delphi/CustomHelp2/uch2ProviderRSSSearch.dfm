object ch2FormConfigRSSSearch: Tch2FormConfigRSSSearch
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Configure RSS Search'
  ClientHeight = 472
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 441
    Width = 620
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 542
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 614
    Height = 410
    Align = alClient
    Caption = 'URL'#39's'
    TabOrder = 1
    object LV: TListView
      AlignWithMargins = True
      Left = 5
      Top = 54
      Width = 604
      Height = 294
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'URL'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = LVSelectItem
    end
    object Panel2: TPanel
      Left = 2
      Top = 351
      Width = 610
      Height = 57
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 9
        Top = 6
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object Label3: TLabel
        Left = 217
        Top = 6
        Width = 23
        Height = 13
        Caption = 'URL:'
      end
      object Label8: TLabel
        Left = 46
        Top = 57
        Width = 3
        Height = 13
        WordWrap = True
      end
      object Label9: TLabel
        Left = 416
        Top = 6
        Width = 41
        Height = 13
        Caption = 'Open in:'
      end
      object ed_Name: TEdit
        Left = 46
        Top = 3
        Width = 154
        Height = 21
        TabOrder = 0
        OnChange = ed_NameChange
      end
      object ed_URL: TEdit
        Left = 246
        Top = 3
        Width = 154
        Height = 21
        Hint = '"$(HelpString)" will be replaced with the helpstring'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = ed_URLChange
      end
      object com_Location: TComboBox
        Left = 463
        Top = 3
        Width = 142
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = com_LocationChange
      end
      inline frame_Deco: Tch2FrameHelpItemDecoration
        Left = 0
        Top = 29
        Width = 610
        Height = 28
        Align = alBottom
        TabOrder = 3
        TabStop = True
        ExplicitTop = 29
        ExplicitWidth = 610
        inherited Label1: TLabel
          Left = 331
          ExplicitLeft = 331
        end
        inherited Label2: TLabel
          Left = 194
          ExplicitLeft = 194
        end
        inherited lbl_Caption: TLabel
          Width = 185
          Caption = 'Sample entry'
          ExplicitWidth = 63
        end
        inherited cb_Bold: TCheckBox
          Left = 467
          Width = 31
          ExplicitLeft = 467
          ExplicitWidth = 31
        end
        inherited cb_Italic: TCheckBox
          Left = 504
          ExplicitLeft = 504
        end
        inherited cb_Underline: TCheckBox
          Left = 540
          Width = 31
          ExplicitLeft = 540
          ExplicitWidth = 31
        end
        inherited cb_Strike: TCheckBox
          Left = 577
          ExplicitLeft = 577
        end
        inherited cob_Text: TColorBox
          Left = 226
          Width = 99
          ExplicitLeft = 226
          ExplicitWidth = 99
        end
        inherited cob_Back: TColorBox
          Left = 363
          ExplicitLeft = 363
        end
      end
    end
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 604
      Height = 30
      ButtonHeight = 30
      ButtonWidth = 31
      Caption = 'ToolBar1'
      Images = ch2Data.ch2Images24
      TabOrder = 2
      object btn_Add: TToolButton
        Left = 0
        Top = 0
        Caption = 'btn_Add'
        ImageIndex = 1
        OnClick = btn_AddClick
      end
      object btn_Del: TToolButton
        Left = 31
        Top = 0
        Caption = 'btn_Del'
        ImageIndex = 2
        OnClick = btn_DelClick
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 620
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 38
      Height = 19
      Align = alLeft
      Caption = 'Priority:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object ed_Prio: TSpinEdit
      Left = 54
      Top = 3
      Width = 59
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
  end
end
