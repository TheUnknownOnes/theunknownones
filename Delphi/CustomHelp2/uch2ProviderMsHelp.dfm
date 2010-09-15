object ch2FormProviderMsHelp: Tch2FormProviderMsHelp
  Left = 0
  Top = 0
  Caption = 'MS Help 2.x'
  ClientHeight = 412
  ClientWidth = 605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 12
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 65
    Width = 597
    Height = 313
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
    object lvNamespaces: TListView
      AlignWithMargins = True
      Left = 6
      Top = 19
      Width = 585
      Height = 238
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
      Top = 264
      Width = 587
      Height = 44
      Align = alBottom
      Caption = 'Options'
      TabOrder = 1
      inline FrameHelpItemDeco: Tch2FrameHelpItemDecoration
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 577
        Height = 21
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 5
        ExplicitTop = 18
        ExplicitWidth = 577
        ExplicitHeight = 21
        inherited Label1: TLabel
          Left = 328
          Height = 15
          ExplicitLeft = 328
        end
        inherited Label2: TLabel
          Left = 217
          Height = 15
          ExplicitLeft = 217
        end
        inherited lbl_Caption: TLabel
          Width = 208
          Height = 15
          Caption = 'Sample Header'
          ExplicitWidth = 72
        end
        inherited cb_Bold: TCheckBox
          Left = 439
          Width = 29
          Height = 15
          ExplicitLeft = 439
          ExplicitWidth = 29
          ExplicitHeight = 15
        end
        inherited cb_Italic: TCheckBox
          Left = 474
          Height = 15
          ExplicitLeft = 474
          ExplicitHeight = 15
        end
        inherited cb_Underline: TCheckBox
          Left = 510
          Width = 29
          Height = 15
          ExplicitLeft = 510
          ExplicitWidth = 29
          ExplicitHeight = 15
        end
        inherited cb_Strike: TCheckBox
          Left = 545
          Width = 29
          Height = 15
          ExplicitLeft = 545
          ExplicitWidth = 29
          ExplicitHeight = 15
        end
        inherited cob_Text: TColorBox
          Left = 249
          Width = 73
          ExplicitLeft = 249
          ExplicitWidth = 73
        end
        inherited cob_Back: TColorBox
          Left = 360
          Width = 73
          ExplicitLeft = 360
          ExplicitWidth = 73
        end
      end
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 597
    Height = 53
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Priority'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object ed_Prio: TSpinEdit
      Left = 11
      Top = 20
      Width = 69
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
  end
  object Panel1: TPanel
    Left = 0
    Top = 382
    Width = 605
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 528
      Top = 4
      Width = 73
      Height = 22
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
