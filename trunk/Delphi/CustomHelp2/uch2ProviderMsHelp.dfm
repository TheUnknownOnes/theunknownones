object ch2FormProviderMsHelp: Tch2FormProviderMsHelp
  Left = 0
  Top = 0
  Caption = 'MS Help 2.x'
  ClientHeight = 549
  ClientWidth = 812
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 802
    Height = 499
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Caption = 'MS Help Namespaces'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lvNameSpaces: TListView
      AlignWithMargins = True
      Left = 7
      Top = 25
      Width = 788
      Height = 374
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 251
        end
        item
          AutoSize = True
          Caption = 'Description'
        end
        item
          Caption = 'Search Type'
          Width = 180
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvNameSpacesChange
    end
    object GroupBox3: TGroupBox
      AlignWithMargins = True
      Left = 6
      Top = 408
      Width = 790
      Height = 85
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alBottom
      Caption = 'Options'
      TabOrder = 1
      inline FrameHelpItemDeco: Tch2FrameHelpItemDecoration
        AlignWithMargins = True
        Left = 6
        Top = 53
        Width = 778
        Height = 28
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 6
        ExplicitTop = 53
        ExplicitWidth = 778
        inherited Label1: TLabel
          Left = 445
          Top = 4
          Width = 36
          Height = 20
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 445
          ExplicitTop = 4
          ExplicitWidth = 36
          ExplicitHeight = 18
        end
        inherited Label2: TLabel
          Left = 296
          Top = 4
          Width = 36
          Height = 20
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 296
          ExplicitTop = 4
          ExplicitWidth = 36
          ExplicitHeight = 18
        end
        inherited lbl_Caption: TLabel
          Left = 4
          Top = 4
          Width = 284
          Height = 20
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Sample Header'
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitWidth = 99
          ExplicitHeight = 18
        end
        inherited cb_Bold: TCheckBox
          Left = 594
          Top = 4
          Width = 39
          Height = 20
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 594
          ExplicitTop = 4
          ExplicitWidth = 39
          ExplicitHeight = 20
        end
        inherited cb_Italic: TCheckBox
          Left = 641
          Top = 4
          Width = 40
          Height = 20
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 641
          ExplicitTop = 4
          ExplicitWidth = 40
          ExplicitHeight = 20
        end
        inherited cb_Underline: TCheckBox
          Left = 689
          Top = 4
          Width = 39
          Height = 20
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 689
          ExplicitTop = 4
          ExplicitWidth = 39
          ExplicitHeight = 20
        end
        inherited cb_Strike: TCheckBox
          Left = 736
          Top = 4
          Width = 38
          Height = 20
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
          ExplicitLeft = 736
          ExplicitTop = 4
          ExplicitWidth = 38
          ExplicitHeight = 20
        end
        inherited cob_Text: TColorBox
          Left = 340
          Top = 4
          Width = 97
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 340
          ExplicitTop = 4
          ExplicitWidth = 97
        end
        inherited cob_Back: TColorBox
          Left = 489
          Top = 4
          Width = 97
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 489
          ExplicitTop = 4
          ExplicitWidth = 97
        end
      end
      object Panel2: TPanel
        Left = 2
        Top = 20
        Width = 786
        Height = 29
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 3
          Width = 83
          Height = 18
          Caption = 'Search Type'
        end
        object Label2: TLabel
          Left = 379
          Top = -4
          Width = 354
          Height = 28
          Caption = 
            'note: Not all search types may be supported by the namespace.'#13#10'Y' +
            'ou may enter a user defined indexname if you wish'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbSearchType: TComboBox
          Left = 112
          Top = 0
          Width = 249
          Height = 26
          ItemHeight = 18
          TabOrder = 0
          OnChange = cbSearchTypeChange
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 509
    Width = 812
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 710
      Top = 5
      Width = 97
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Cancel = True
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
