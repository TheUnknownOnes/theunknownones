object ch2FormConfigGoogleCodeSearch: Tch2FormConfigGoogleCodeSearch
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Google Codesearch'
  ClientHeight = 422
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 114
    Width = 619
    Height = 274
    Align = alClient
    Caption = 'Queries'
    TabOrder = 0
    object LV: TListView
      AlignWithMargins = True
      Left = 5
      Top = 54
      Width = 609
      Height = 138
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'Query'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = LVSelectItem
      ExplicitTop = 46
      ExplicitHeight = 146
    end
    object Panel2: TPanel
      Left = 2
      Top = 195
      Width = 615
      Height = 77
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
        Left = 206
        Top = 6
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
        AlignWithMargins = True
        Left = 3
        Top = 61
        Width = 609
        Height = 13
        Cursor = crHandPoint
        Align = alBottom
        Alignment = taCenter
        Caption = 'Click here to build up a valid query'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHotLight
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = False
        OnClick = Label4Click
        ExplicitWidth = 164
      end
      object Label9: TLabel
        Left = 409
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
      object ed_Query: TEdit
        Left = 246
        Top = 3
        Width = 157
        Height = 21
        Hint = '"$(HelpString)" will be replaced with the helpstring'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = ed_QueryChange
      end
      inline frame_Deco: Tch2FrameHelpItemDecoration
        Left = 0
        Top = 30
        Width = 615
        Height = 28
        Align = alBottom
        TabOrder = 2
        ExplicitTop = 30
        ExplicitWidth = 615
        inherited Label1: TLabel
          Left = 336
          ExplicitLeft = 336
        end
        inherited Label2: TLabel
          Left = 200
          ExplicitLeft = 200
        end
        inherited lbl_Caption: TLabel
          Width = 191
          Caption = 'Sample entry'
          ExplicitWidth = 63
        end
        inherited cb_Bold: TCheckBox
          Left = 472
          Width = 31
          ExplicitLeft = 472
          ExplicitWidth = 31
        end
        inherited cb_Italic: TCheckBox
          Left = 509
          ExplicitLeft = 509
        end
        inherited cb_Underline: TCheckBox
          Left = 545
          Width = 31
          ExplicitLeft = 545
          ExplicitWidth = 31
        end
        inherited cb_Strike: TCheckBox
          Left = 582
          ExplicitLeft = 582
        end
        inherited cob_Text: TColorBox
          Left = 232
          ExplicitLeft = 232
        end
        inherited cob_Back: TColorBox
          Left = 368
          ExplicitLeft = 368
        end
      end
      object com_Location: TComboBox
        Left = 456
        Top = 3
        Width = 151
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = com_LocationChange
      end
    end
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 609
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
    Top = 391
    Width = 625
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 547
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
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 619
    Height = 105
    Align = alTop
    Caption = 'General'
    TabOrder = 2
    DesignSize = (
      619
      105)
    object Label1: TLabel
      AlignWithMargins = True
      Left = 29
      Top = 18
      Width = 38
      Height = 13
      Caption = 'Priority:'
      Layout = tlCenter
    end
    object Label5: TLabel
      Left = 16
      Top = 46
      Width = 51
      Height = 13
      Caption = 'Feed-URL:'
    end
    object Label6: TLabel
      Left = 16
      Top = 73
      Width = 49
      Height = 13
      Caption = 'Web-URL:'
    end
    object ed_Prio: TSpinEdit
      Left = 73
      Top = 15
      Width = 59
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = ed_PrioChange
    end
    object ed_FeedURL: TEdit
      Left = 73
      Top = 43
      Width = 536
      Height = 21
      Hint = 'The query will be appended'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = ed_FeedURLChange
    end
    object ed_WebURL: TEdit
      Left = 73
      Top = 70
      Width = 536
      Height = 21
      Hint = 'The query will be appended'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnChange = ed_WebURLChange
    end
  end
end
