object ch2FormConfigGoogleCodeSearch: Tch2FormConfigGoogleCodeSearch
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Google Codesearch'
  ClientHeight = 552
  ClientWidth = 817
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 149
    Width = 809
    Height = 358
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Caption = 'Queries'
    TabOrder = 0
    object LV: TListView
      AlignWithMargins = True
      Left = 6
      Top = 59
      Width = 797
      Height = 192
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 131
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
      ExplicitLeft = 7
      ExplicitTop = 71
      ExplicitWidth = 796
      ExplicitHeight = 180
    end
    object Panel2: TPanel
      Left = 2
      Top = 255
      Width = 805
      Height = 101
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 3
      ExplicitWidth = 804
      object Label2: TLabel
        Left = 12
        Top = 8
        Width = 40
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Name:'
      end
      object Label3: TLabel
        Left = 269
        Top = 8
        Width = 43
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Query:'
      end
      object Label8: TLabel
        Left = 60
        Top = 75
        Width = 4
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        WordWrap = True
      end
      object Label4: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 79
        Width = 797
        Height = 18
        Cursor = crHandPoint
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alBottom
        Alignment = taCenter
        Caption = 'Click here to build up a valid query'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHotLight
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = False
        OnClick = Label4Click
        ExplicitWidth = 221
      end
      object Label9: TLabel
        Left = 535
        Top = 8
        Width = 52
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Open in:'
      end
      object ed_Name: TEdit
        Left = 60
        Top = 4
        Width = 202
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
        OnChange = ed_NameChange
      end
      object ed_Query: TEdit
        Left = 322
        Top = 4
        Width = 205
        Height = 21
        Hint = '"$(HelpString)" will be replaced with the helpstring'
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = ed_QueryChange
      end
      inline frame_Deco: Tch2FrameHelpItemDecoration
        Left = 0
        Top = 38
        Width = 805
        Height = 37
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alBottom
        TabOrder = 2
        TabStop = True
        ExplicitTop = 39
        ExplicitWidth = 804
        ExplicitHeight = 37
        inherited Label1: TLabel
          Left = 440
          Top = 4
          Width = 34
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 430
          ExplicitTop = 4
          ExplicitWidth = 34
          ExplicitHeight = 17
        end
        inherited Label2: TLabel
          Left = 263
          Top = 4
          Width = 33
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 253
          ExplicitTop = 4
          ExplicitWidth = 33
          ExplicitHeight = 17
        end
        inherited lbl_Caption: TLabel
          Left = 4
          Top = 4
          Width = 251
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Sample entry'
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitWidth = 81
          ExplicitHeight = 17
        end
        inherited cb_Bold: TCheckBox
          Left = 618
          Top = 4
          Width = 41
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 617
          ExplicitTop = 4
          ExplicitWidth = 41
          ExplicitHeight = 29
        end
        inherited cb_Italic: TCheckBox
          Left = 667
          Top = 4
          Width = 39
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 666
          ExplicitTop = 4
          ExplicitWidth = 39
          ExplicitHeight = 29
        end
        inherited cb_Underline: TCheckBox
          Left = 714
          Top = 4
          Width = 40
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 713
          ExplicitTop = 4
          ExplicitWidth = 40
          ExplicitHeight = 29
        end
        inherited cb_Strike: TCheckBox
          Left = 762
          Top = 4
          Width = 39
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 761
          ExplicitTop = 4
          ExplicitWidth = 39
          ExplicitHeight = 29
        end
        inherited cob_Text: TColorBox
          Left = 304
          Top = 4
          Width = 128
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 293
          ExplicitTop = 4
          ExplicitWidth = 128
        end
        inherited cob_Back: TColorBox
          Left = 482
          Top = 4
          Width = 128
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 481
          ExplicitTop = 4
          ExplicitWidth = 128
        end
      end
      object com_Location: TComboBox
        Left = 596
        Top = 4
        Width = 198
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 3
        OnChange = com_LocationChange
      end
    end
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 5
      Top = 22
      Width = 799
      Height = 30
      ButtonHeight = 30
      ButtonWidth = 31
      Caption = 'ToolBar1'
      Images = ch2Data.ch2Images24
      TabOrder = 2
      ExplicitTop = 18
      ExplicitWidth = 609
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
    Top = 511
    Width = 817
    Height = 41
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 715
      Top = 4
      Width = 98
      Height = 33
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 809
    Height = 137
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Caption = 'General'
    TabOrder = 2
    DesignSize = (
      809
      137)
    object Label1: TLabel
      AlignWithMargins = True
      Left = 38
      Top = 24
      Width = 48
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Priority:'
      Layout = tlCenter
    end
    object Label5: TLabel
      Left = 21
      Top = 60
      Width = 64
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Feed-URL:'
    end
    object Label6: TLabel
      Left = 21
      Top = 95
      Width = 64
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Web-URL:'
    end
    object ed_Prio: TSpinEdit
      Left = 95
      Top = 20
      Width = 78
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = ed_PrioChange
    end
    object ed_FeedURL: TEdit
      Left = 95
      Top = 56
      Width = 701
      Height = 21
      Hint = 'The query will be appended'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = ed_FeedURLChange
    end
    object ed_WebURL: TEdit
      Left = 95
      Top = 92
      Width = 701
      Height = 21
      Hint = 'The query will be appended'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnChange = ed_WebURLChange
    end
  end
end
