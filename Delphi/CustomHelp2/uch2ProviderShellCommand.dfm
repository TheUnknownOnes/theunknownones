object ch2FormConfigShellCommand: Tch2FormConfigShellCommand
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure ShellCommand Help'
  ClientHeight = 373
  ClientWidth = 570
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
  object Panel2: TPanel
    Left = 0
    Top = 342
    Width = 570
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btn_Ok: TButton
      AlignWithMargins = True
      Left = 492
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
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
    Width = 564
    Height = 336
    Align = alClient
    Caption = 'Commands'
    TabOrder = 1
    object lv: TListView
      Left = 2
      Top = 51
      Width = 560
      Height = 221
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          AutoSize = True
          Caption = 'Command'
        end
        item
          Caption = 'Params'
          Width = 200
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvSelectItem
    end
    object Panel3: TPanel
      Left = 2
      Top = 272
      Width = 560
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      inline frame_Deco: Tch2FrameHelpItemDecoration
        Left = 0
        Top = 34
        Width = 560
        Height = 28
        Align = alBottom
        TabOrder = 0
        TabStop = True
        ExplicitTop = 34
        ExplicitWidth = 560
        inherited Label1: TLabel
          Left = 283
          ExplicitLeft = 283
        end
        inherited Label2: TLabel
          Left = 147
          ExplicitLeft = 147
        end
        inherited lbl_Caption: TLabel
          Width = 138
          Caption = 'Sample entry'
          ExplicitWidth = 63
        end
        inherited cb_Bold: TCheckBox
          Left = 419
          ExplicitLeft = 419
        end
        inherited cb_Italic: TCheckBox
          Left = 455
          ExplicitLeft = 455
        end
        inherited cb_Underline: TCheckBox
          Left = 491
          ExplicitLeft = 491
        end
        inherited cb_Strike: TCheckBox
          Left = 527
          ExplicitLeft = 527
        end
        inherited cob_Text: TColorBox
          Left = 179
          ExplicitLeft = 179
        end
        inherited cob_Back: TColorBox
          Left = 315
          ExplicitLeft = 315
        end
      end
      object ed_Name: TLabeledEdit
        Left = 41
        Top = 6
        Width = 121
        Height = 21
        EditLabel.Width = 31
        EditLabel.Height = 13
        EditLabel.Caption = 'Name:'
        LabelPosition = lpLeft
        TabOrder = 1
        OnChange = ed_NameChange
      end
      object ed_Command: TLabeledEdit
        Left = 225
        Top = 6
        Width = 136
        Height = 21
        Hint = '"$(HelpString)" will be replaced with the helpstring'
        EditLabel.Width = 51
        EditLabel.Height = 13
        EditLabel.Caption = 'Command:'
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnChange = ed_CommandChange
      end
      object ed_Params: TLabeledEdit
        Left = 408
        Top = 6
        Width = 146
        Height = 21
        Hint = '"$(HelpString)" will be replaced with the helpstring'
        EditLabel.Width = 39
        EditLabel.Height = 13
        EditLabel.Caption = 'Params:'
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnChange = ed_ParamsChange
      end
    end
    object TB: TToolBar
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 554
      Height = 30
      ButtonHeight = 30
      ButtonWidth = 31
      Caption = 'TB'
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
end
