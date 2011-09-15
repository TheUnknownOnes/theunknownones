object ch2FormProviderMsHelp: Tch2FormProviderMsHelp
  Left = 0
  Top = 0
  Caption = 'MS Help 2.x'
  ClientHeight = 412
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 601
    Height = 374
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Caption = 'MS Help Namespaces'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lvNameSpaces: TListView
      AlignWithMargins = True
      Left = 6
      Top = 19
      Width = 589
      Height = 273
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 188
        end
        item
          AutoSize = True
          Caption = 'Description'
        end
        item
          Caption = 'Search Type'
          Width = 135
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvNameSpacesChange
      ExplicitHeight = 279
    end
    object GroupBox3: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 299
      Width = 591
      Height = 70
      Align = alBottom
      Caption = 'Options'
      TabOrder = 1
      inline FrameHelpItemDeco: Tch2FrameHelpItemDecoration
        AlignWithMargins = True
        Left = 5
        Top = 40
        Width = 581
        Height = 24
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 5
        ExplicitTop = 40
        ExplicitWidth = 581
        ExplicitHeight = 24
        inherited Label1: TLabel
          Left = 332
          Height = 13
          ExplicitLeft = 332
        end
        inherited Label2: TLabel
          Left = 221
          Height = 13
          ExplicitLeft = 221
        end
        inherited lbl_Caption: TLabel
          Width = 72
          Height = 13
          Caption = 'Sample Header'
          ExplicitWidth = 72
        end
        inherited cb_Bold: TCheckBox
          Left = 443
          Width = 29
          Height = 18
          ExplicitLeft = 443
          ExplicitWidth = 29
          ExplicitHeight = 15
        end
        inherited cb_Italic: TCheckBox
          Left = 478
          Height = 18
          ExplicitLeft = 478
          ExplicitHeight = 15
        end
        inherited cb_Underline: TCheckBox
          Left = 514
          Width = 29
          Height = 18
          ExplicitLeft = 514
          ExplicitWidth = 29
          ExplicitHeight = 15
        end
        inherited cb_Strike: TCheckBox
          Left = 549
          Width = 29
          Height = 18
          ExplicitLeft = 549
          ExplicitWidth = 29
          ExplicitHeight = 15
        end
        inherited cob_Text: TColorBox
          Left = 253
          Width = 73
          ExplicitLeft = 253
          ExplicitWidth = 73
        end
        inherited cob_Back: TColorBox
          Left = 364
          Width = 73
          ExplicitLeft = 364
          ExplicitWidth = 73
        end
      end
      object Panel2: TPanel
        Left = 2
        Top = 15
        Width = 587
        Height = 22
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 6
          Top = 2
          Width = 60
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Search Type'
        end
        object Label2: TLabel
          Left = 284
          Top = -3
          Width = 266
          Height = 22
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 
            'note: Not all search types may be supported by the namespace.'#13#10'Y' +
            'ou may enter a user defined indexname if you wish'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbSearchType: TComboBox
          Left = 84
          Top = 0
          Width = 187
          Height = 21
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          ItemHeight = 0
          TabOrder = 0
          OnChange = cbSearchTypeChange
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 382
    Width = 609
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 533
      Top = 4
      Width = 72
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Cancel = True
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
