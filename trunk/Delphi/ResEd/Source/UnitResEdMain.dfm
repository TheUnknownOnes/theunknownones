object FormWizardResEd: TFormWizardResEd
  Left = 453
  Top = 377
  BorderWidth = 3
  Caption = 'ResEd (Project Resources)'
  ClientHeight = 327
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001001800680300001600000028000000100000002000
    0000010018000000000040030000000000000000000000000000000000000000
    00A84814A43E059E3A009A340099330000000000000000000099330099330099
    3300000000000000000000000000000000AE5722F43521EB1E10E61000DE0E00
    993300000000993300C41800C11A009933000000000000000000000000000000
    00B26436FC463EF72B1DF21102993300000000000000000000993300C5170099
    3300000000000000000000000000000000B66E3FFE5650FD3B34FB19119B3703
    000000000000000000993300D013009933000000000000000000000000000000
    00BA754BFF665EFF4E49FF2822FE110B9D39079B37039A3401E90900DB0D0099
    3300000000993300993300000000000000BF7D51FF7B71B4673AA8531FFF221F
    FF1916FE1513FB0E0CF60702EB0701D70F00993300C21900BD1C009933000000
    00000000C08256000000000000A8501DFF2722FF211FFE1A19FB110FF60C09E8
    0E05D31502C61900C31900993300000000000000000000000000000000B16131
    FF3E3AFF302EFF2927FF201EFB1A15F417139D3B06D61E09CB1E069A35010000
    00000000CE9877000000000000BB7344FF5E54FF4D46FF433DFF3B36FF332DA3
    4C17000000A44611A2450E000000000000CE9B72FFB6ADD1A27ACF9C71FF9A8D
    FF857AFF766AFF665FFF6059FF5955B161300000000000000000000000000000
    00CC966EFFB7A9FFBCB1FFBCAFFFB2A6FFA499FF998BFF8C82FF857AFF8177BD
    7A52000000000000000000000000000000C3855ACF9B73D9AB85D9AF8BD8B089
    FFC0B0FFB5A8FFAB9FC69367C88D65C58C610000000000000000000000000000
    00000000000000000000000000000000DCB795FFC7B9D5A77E00000000000000
    0000000000000000000000000000000000000000000000000000000000DBB691
    F8FAEDFFCCBFFFC6B9D7A6840000000000000000000000000000000000000000
    00000000000000000000000000D3A07BFAEEE2FAEEE1FFC4B4D6A58000000000
    0000000000000000000000000000000000000000000000000000000000000000
    CB9569D09870CE9C73000000000000000000000000000000000000000000838F
    0000810F0000838F0000838F00008009000080000000D8000000F8000000D809
    0000800F0000800F0000800F0000FC7F0000F83F0000F83F0000FC7F0000}
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object PopupEditor: TPopupMenu
    OnPopup = PopupEditorPopup
    Left = 104
    Top = 88
    object miCreateResourceFile: TMenuItem
      Caption = '&Create Resource File'
      OnClick = miCreateResourceFileClick
    end
    object miNewResource: TMenuItem
      Caption = '&New Resource'
      object miICON: TMenuItem
        Caption = 'Icon'
        OnClick = CreateResourceClick
      end
      object miBITMAP: TMenuItem
        Caption = 'Bitmap'
        OnClick = CreateResourceClick
      end
      object miCURSOR: TMenuItem
        Caption = 'Cursor'
        OnClick = CreateResourceClick
      end
      object miGIF: TMenuItem
        Caption = 'GIF Image'
        OnClick = CreateResourceClick
      end
      object miJPEG: TMenuItem
        Caption = 'JPEG Image'
        OnClick = CreateResourceClick
      end
      object miPNG: TMenuItem
        Caption = 'PNG Image'
        OnClick = CreateResourceClick
      end
      object miRCDATA: TMenuItem
        Caption = 'RC Data'
        OnClick = CreateResourceClick
      end
      object miUserData: TMenuItem
        Caption = 'User Data'
        OnClick = CreateResourceClick
      end
      object miStringTable: TMenuItem
        Caption = 'String Table'
        OnClick = CreateResourceClick
      end
      object miMessageTable: TMenuItem
        Caption = 'Message Table'
        OnClick = CreateResourceClick
      end
      object miVersionInfo: TMenuItem
        Caption = 'Version Info'
        OnClick = CreateResourceClick
      end
      object miDotNetCustomData: TMenuItem
        Caption = 'DotNet Custom Data'
        OnClick = CreateResourceClick
      end
      object miManifest: TMenuItem
        Caption = 'XP/Vista Manifest'
        OnClick = CreateResourceClick
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miChange: TMenuItem
      Caption = '&Modify Resource'
      Default = True
      OnClick = miChangeClick
    end
    object miSaveToFile: TMenuItem
      Caption = '&Save Resource to File'
      OnClick = miSaveToFileClick
    end
    object miRename: TMenuItem
      Caption = '&Rename Resource'
      ShortCut = 113
      OnClick = miRenameClick
    end
    object miResourceLanguage: TMenuItem
      Caption = 'Resource &Language'
      object currentLanguage1: TMenuItem
        Caption = 'current Language'
        Enabled = False
      end
      object miCurrentResLang: TMenuItem
        Caption = '...'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Sprachendernin1: TMenuItem
        Caption = 'change Language to'
        Enabled = False
      end
      object miSetNeutral: TMenuItem
        Caption = 'Neutral Lang'
        OnClick = SetResLang
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miRefresh: TMenuItem
      Caption = 'Refresh List'
      ShortCut = 116
      OnClick = miRefreshClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object miDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = miDeleteClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object miAddToSrc: TMenuItem
      Caption = '&Add Resource To Source'
      OnClick = miAddToSrcClick
    end
  end
end
