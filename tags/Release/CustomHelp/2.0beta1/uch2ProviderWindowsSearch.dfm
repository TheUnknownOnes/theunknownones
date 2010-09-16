object uch2FormProviderWindowsSearch: Tuch2FormProviderWindowsSearch
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Config Windows Search'
  ClientHeight = 375
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 650
    Height = 313
    Align = alClient
    Caption = 'Searches'
    TabOrder = 0
    object LV: TListView
      AlignWithMargins = True
      Left = 5
      Top = 54
      Width = 640
      Height = 197
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'Query'
        end
        item
          Alignment = taRightJustify
          Caption = 'max Results'
          Width = 76
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LVChange
    end
    object Panel2: TPanel
      Left = 2
      Top = 254
      Width = 646
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
        Left = 207
        Top = 5
        Width = 34
        Height = 13
        Caption = 'Query:'
      end
      object Label8: TLabel
        Left = 46
        Top = 57
        Width = 3
        Height = 13
        WordWrap = True
      end
      object Label4: TLabel
        Left = 515
        Top = 5
        Width = 62
        Height = 13
        Caption = 'max Results:'
      end
      object ed_Name: TEdit
        Left = 46
        Top = 3
        Width = 154
        Height = 21
        TabOrder = 0
        OnChange = ed_NameChange
      end
      object ed_Query: TEdit
        Left = 246
        Top = 3
        Width = 263
        Height = 21
        Hint = '"$(HelpString)" will be replaced with the helpstring'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = ed_QueryChange
      end
      inline frame_Deco: Tch2FrameHelpItemDecoration
        Left = 0
        Top = 29
        Width = 646
        Height = 28
        Align = alBottom
        TabOrder = 2
        TabStop = True
        ExplicitTop = 29
        ExplicitWidth = 646
        inherited Label1: TLabel
          Left = 368
          ExplicitLeft = 368
        end
        inherited Label2: TLabel
          Left = 231
          ExplicitLeft = 231
        end
        inherited lbl_Caption: TLabel
          Width = 222
          Caption = 'Sample entry'
          ExplicitWidth = 63
        end
        inherited cb_Bold: TCheckBox
          Left = 504
          Width = 31
          ExplicitLeft = 504
          ExplicitWidth = 31
        end
        inherited cb_Italic: TCheckBox
          Left = 541
          Width = 29
          ExplicitLeft = 541
          ExplicitWidth = 29
        end
        inherited cb_Underline: TCheckBox
          Left = 576
          Width = 31
          ExplicitLeft = 576
          ExplicitWidth = 31
        end
        inherited cb_Strike: TCheckBox
          Left = 613
          ExplicitLeft = 613
        end
        inherited cob_Text: TColorBox
          Left = 263
          Width = 99
          ExplicitLeft = 263
          ExplicitWidth = 99
        end
        inherited cob_Back: TColorBox
          Left = 400
          ExplicitLeft = 400
        end
      end
      object ed_maxResults: TSpinEdit
        Left = 580
        Top = 2
        Width = 63
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = ed_maxResultsChange
      end
    end
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 640
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
  object Panel1: TPanel
    Left = 0
    Top = 344
    Width = 656
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 578
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btn_OKClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 656
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
