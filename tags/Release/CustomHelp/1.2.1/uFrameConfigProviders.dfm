object FrameConfigProviders: TFrameConfigProviders
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  TabStop = True
  object pnlOHSItem: TPanel
    Left = 0
    Top = 210
    Width = 451
    Height = 94
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      451
      94)
    object Label1: TLabel
      Left = 6
      Top = 6
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object Label2: TLabel
      Left = 6
      Top = 29
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object LabelURLPath: TLabel
      Left = 6
      Top = 52
      Width = 16
      Height = 13
      Caption = '%s'
    end
    object Label8: TLabel
      Left = 6
      Top = 73
      Width = 82
      Height = 13
      Caption = 'Trim namespaces'
    end
    object BtnBrowseForFile: TSpeedButton
      Left = 423
      Top = 48
      Width = 23
      Height = 22
      Anchors = [akTop, akRight]
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        08000000000000010000420B0000420B0000000100000001000000730800087B
        080008841000088C100008A51800108C2100109C210018AD290031C64A0042D6
        6B0052D67B005AE78C0018A5C60018ADD60021ADD60029ADD60031B5DE0052BD
        E7004AC6E7004AC6EF009CDEEF00ADDEEF006BDEF70073DEF700A5EFF700FF00
        FF0084EFFF008CEFFF0094EFFF008CF7FF0094F7FF00A5F7FF0094FFFF009CFF
        FF00ADFFFF00C6FFFF00D6FFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00191919191919
        19191919191919191919190F100E191919191919191919191919190F141A120E
        0C0C0C19191919191919190F11212017171717120E0C19191919190F11221D1B
        1B1B171717130E191919190F0F151E1E1B1B1B1B171713191919190F170F211D
        1D1D1B1B1B17170C1919190F1E0F1518181F1B1B1B17000C1919190F21170F0C
        0C0C151D1A000B000C19190F211E171717160F15000A09080019190F211E1E1E
        1E17170F0C0508060C19190F23202124241B1C17170207021919190E14232314
        0D0C0C0C0C03041919191919100F0C0C19191919030402191919191919191919
        1900010303011919191919191919191919191919191919191919}
      OnClick = BtnBrowseForFileClick
      ExplicitLeft = 662
    end
    object edName: TEdit
      Left = 94
      Top = 3
      Width = 355
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 0
      OnChange = edNameChange
    end
    object edDesc: TEdit
      Left = 94
      Top = 26
      Width = 355
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 1
      OnChange = edDescChange
    end
    object edURL: TEdit
      Left = 94
      Top = 48
      Width = 328
      Height = 21
      Hint = 
        '- URL to a webbased search provider (e.g. koders.com)'#13#10'- Path to' +
        ' a windows *.hlp file (be aware to have winhlp32.exe installed)'#13 +
        #10'- Path to a windows htmlHelp file (*.chm)'
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnChange = edURLChange
    end
    object cbTrimNamespacesOHS: TComboBox
      Left = 94
      Top = 70
      Width = 145
      Height = 21
      Hint = 
        'Example:'#13#10'Full Keyword is  Classes.TStringList.Create'#13#10'Trim Firs' +
        't searches for TStringList.Create'#13#10'Trim Full searches for Create'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnChange = cbTrimNamespacesOHSChange
    end
  end
  object ListView1: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 445
    Height = 204
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Description'
        Width = 100
      end
      item
        AutoSize = True
        Caption = '%s ... $(HelpString) will be replaced by Keyword'
      end
      item
        Caption = 'Trim Namespaces'
        Width = 80
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListView1Change
    OnDblClick = ListView1DblClick
    OnKeyDown = ListView1KeyDown
  end
  object OpenDialog1: TOpenDialog
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 208
    Top = 136
  end
end
