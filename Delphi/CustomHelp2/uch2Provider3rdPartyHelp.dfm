object ch2FormProvider3rdPartyHelp: Tch2FormProvider3rdPartyHelp
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure 3rd Party Help Viewers for use in CustomHelp2'
  ClientHeight = 576
  ClientWidth = 903
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 17
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 41
    Width = 897
    Height = 428
    Align = alClient
    Caption = 'Currently installed Help Viewers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitHeight = 393
    object clbProviders: TCheckListBox
      AlignWithMargins = True
      Left = 5
      Top = 23
      Width = 887
      Height = 400
      OnClickCheck = clbProvidersClickCheck
      Align = alClient
      ItemHeight = 18
      TabOrder = 0
      OnClick = clbProvidersClick
      ExplicitHeight = 365
    end
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 897
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 11
      Width = 43
      Height = 18
      Caption = 'Priority'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object EditPrio: TSpinEdit
      Left = 64
      Top = 4
      Width = 81
      Height = 28
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxValue = 0
      MinValue = 0
      ParentFont = False
      TabOrder = 0
      Value = 0
      OnChange = EditPrioChange
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 475
    Width = 897
    Height = 58
    Align = alBottom
    Caption = 'Options'
    TabOrder = 2
    ExplicitTop = 440
    inline FrameHelpItemDeco: Tch2FrameHelpItemDecoration
      AlignWithMargins = True
      Left = 5
      Top = 22
      Width = 887
      Height = 28
      Align = alTop
      TabOrder = 0
      ExplicitLeft = 5
      ExplicitTop = 22
      ExplicitWidth = 887
      inherited Label1: TLabel
        Left = 602
        Width = 34
        ExplicitLeft = 602
        ExplicitWidth = 34
        ExplicitHeight = 17
      end
      inherited Label2: TLabel
        Left = 459
        Width = 33
        ExplicitLeft = 459
        ExplicitWidth = 33
        ExplicitHeight = 17
      end
      inherited lbl_Caption: TLabel
        Width = 450
        Caption = 'Sample Header'
        ExplicitWidth = 91
        ExplicitHeight = 17
      end
      inherited cb_Bold: TCheckBox
        Left = 746
        ExplicitLeft = 746
      end
      inherited cb_Italic: TCheckBox
        Left = 782
        ExplicitLeft = 782
      end
      inherited cb_Underline: TCheckBox
        Left = 818
        ExplicitLeft = 818
      end
      inherited cb_Strike: TCheckBox
        Left = 854
        ExplicitLeft = 854
      end
      inherited cob_Text: TColorBox
        Left = 498
        ExplicitLeft = 498
      end
      inherited cob_Back: TColorBox
        Left = 642
        ExplicitLeft = 642
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 536
    Width = 903
    Height = 40
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 461
    ExplicitWidth = 811
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 801
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
      ExplicitLeft = 709
    end
  end
end
