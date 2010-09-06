object ch2FormProviderMsHelp: Tch2FormProviderMsHelp
  Left = 0
  Top = 0
  Caption = 'MS Help 2.x'
  ClientHeight = 549
  ClientWidth = 807
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 83
    Width = 799
    Height = 422
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Caption = 'MS Help Namespaces'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lvNamespaces: TListView
      AlignWithMargins = True
      Left = 6
      Top = 23
      Width = 787
      Height = 328
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 250
        end
        item
          AutoSize = True
          Caption = 'Description'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvNamespacesChange
    end
    object GroupBox3: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 358
      Width = 789
      Height = 59
      Align = alBottom
      Caption = 'Options'
      TabOrder = 1
      inline FrameHelpItemDeco: Tch2FrameHelpItemDecoration
        AlignWithMargins = True
        Left = 5
        Top = 22
        Width = 779
        Height = 28
        Align = alTop
        TabOrder = 0
        ExplicitLeft = 5
        ExplicitTop = 22
        ExplicitWidth = 779
        inherited Label1: TLabel
          Left = 458
          Width = 34
          ExplicitLeft = 473
          ExplicitWidth = 34
          ExplicitHeight = 17
        end
        inherited Label2: TLabel
          Left = 315
          Width = 33
          ExplicitLeft = 333
          ExplicitWidth = 33
          ExplicitHeight = 17
        end
        inherited lbl_Caption: TLabel
          Width = 306
          Caption = 'Sample Header'
          ExplicitWidth = 91
          ExplicitHeight = 17
        end
        inherited cb_Bold: TCheckBox
          Left = 602
          Width = 39
          Font.Height = -15
          ExplicitLeft = 602
          ExplicitWidth = 39
        end
        inherited cb_Italic: TCheckBox
          Left = 647
          Width = 39
          Font.Height = -15
          ExplicitLeft = 647
          ExplicitWidth = 39
        end
        inherited cb_Underline: TCheckBox
          Left = 692
          Width = 39
          Font.Height = -15
          ExplicitLeft = 692
          ExplicitWidth = 39
        end
        inherited cb_Strike: TCheckBox
          Left = 737
          Width = 39
          Font.Height = -15
          ExplicitLeft = 737
          ExplicitWidth = 39
        end
        inherited cob_Text: TColorBox
          Left = 354
          ExplicitLeft = 354
        end
        inherited cob_Back: TColorBox
          Left = 498
          ExplicitLeft = 498
        end
      end
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 799
    Height = 71
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Priority'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object ed_Prio: TSpinEdit
      Left = 14
      Top = 27
      Width = 92
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 509
    Width = 807
    Height = 40
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 705
      Top = 4
      Width = 98
      Height = 32
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
end
