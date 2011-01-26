object formCode: TformCode
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Create Pascal-Code'
  ClientHeight = 405
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 681
    Height = 69
    Align = alTop
    Caption = 'Options'
    TabOrder = 0
    object GridPanel1: TGridPanel
      Left = 2
      Top = 15
      Width = 677
      Height = 52
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 150.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Label1
          Row = 0
        end
        item
          Column = 1
          Control = edProcName
          Row = 0
        end
        item
          Column = 0
          Control = Label2
          Row = 1
        end
        item
          Column = 1
          Control = cbSingleResult
          Row = 1
        end>
      RowCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 63
        Top = 3
        Width = 84
        Height = 13
        Align = alClient
        Alignment = taRightJustify
        Caption = 'Procedure-Name:'
        Layout = tlCenter
      end
      object edProcName: TEdit
        AlignWithMargins = True
        Left = 153
        Top = 3
        Width = 521
        Height = 20
        Align = alClient
        TabOrder = 0
        Text = 'RegEx'
        OnChange = edProcNameChange
        ExplicitHeight = 21
      end
      object Label2: TLabel
        AlignWithMargins = True
        Left = 43
        Top = 29
        Width = 104
        Height = 13
        Align = alClient
        Alignment = taRightJustify
        Caption = 'Result is single string:'
        Layout = tlCenter
      end
      object cbSingleResult: TCheckBox
        AlignWithMargins = True
        Left = 153
        Top = 29
        Width = 521
        Height = 20
        Align = alClient
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbSingleResultClick
      end
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 78
    Width = 681
    Height = 324
    Align = alClient
    Caption = 'Code'
    TabOrder = 1
    object edHead: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 671
      Height = 19
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object memBody: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 43
      Width = 671
      Height = 276
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      OnEnter = memBodyEnter
    end
  end
end
