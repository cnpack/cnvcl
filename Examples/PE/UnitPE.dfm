object FormPE: TFormPE
  Left = 192
  Top = 107
  Width = 967
  Height = 563
  Caption = 'PE File Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblPEFile: TLabel
    Left = 16
    Top = 16
    Width = 36
    Height = 13
    Caption = 'PE File:'
  end
  object bvl1: TBevel
    Left = 520
    Top = 16
    Width = 49
    Height = 25
    Shape = bsLeftLine
  end
  object edtPEFile: TEdit
    Left = 64
    Top = 16
    Width = 281
    Height = 21
    TabOrder = 0
  end
  object btnParsePE: TButton
    Left = 536
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Parse Module'
    TabOrder = 1
    OnClick = btnParsePEClick
  end
  object cbbRunModule: TComboBox
    Left = 616
    Top = 16
    Width = 329
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object btnBrowse: TButton
    Left = 352
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object btnParsePEFile: TButton
    Left = 432
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Parse File'
    TabOrder = 4
    OnClick = btnParsePEFileClick
  end
  object pgcPE: TPageControl
    Left = 16
    Top = 48
    Width = 929
    Height = 473
    ActivePage = tsDosHeader
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
    object tsDosHeader: TTabSheet
      Caption = 'Dos Header'
      object mmoDos: TMemo
        Left = 16
        Top = 16
        Width = 313
        Height = 409
        TabOrder = 0
      end
    end
    object tsNtHeader: TTabSheet
      Caption = 'NtHeader'
      ImageIndex = 1
      object mmoFile: TMemo
        Left = 16
        Top = 16
        Width = 313
        Height = 409
        TabOrder = 0
      end
      object mmoOptional: TMemo
        Left = 336
        Top = 16
        Width = 377
        Height = 409
        TabOrder = 1
      end
    end
    object tsSectionHeader: TTabSheet
      Caption = 'SectionHeader'
      ImageIndex = 2
      object mmoSection: TMemo
        Left = 16
        Top = 16
        Width = 313
        Height = 409
        TabOrder = 0
      end
    end
  end
  object dlgOpen1: TOpenDialog
    Left = 264
    Top = 32
  end
end
