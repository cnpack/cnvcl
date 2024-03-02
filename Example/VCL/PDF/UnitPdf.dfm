object FormPDF: TFormPDF
  Left = 193
  Top = 109
  Width = 917
  Height = 647
  Caption = 'PDF'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 24
    Top = 24
    Width = 857
    Height = 569
    ActivePage = tsParsePDF
    TabOrder = 0
    object tsParsePDF: TTabSheet
      Caption = 'Load && Parse PDF'
      object btnParsePDFToken: TButton
        Left = 24
        Top = 16
        Width = 105
        Height = 25
        Caption = 'Parse PDF Tokens'
        TabOrder = 0
        OnClick = btnParsePDFTokenClick
      end
      object mmoPDF: TMemo
        Left = 24
        Top = 56
        Width = 793
        Height = 457
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object btnParsePDFStructure: TButton
        Left = 160
        Top = 16
        Width = 113
        Height = 25
        Caption = 'Parse PDF Structure'
        TabOrder = 2
        OnClick = btnParsePDFStructureClick
      end
      object btnGenSimple: TButton
        Left = 328
        Top = 16
        Width = 113
        Height = 25
        Caption = 'Generate Simple'
        TabOrder = 3
        OnClick = btnGenSimpleClick
      end
      object btnExtractJPG: TButton
        Left = 480
        Top = 16
        Width = 105
        Height = 25
        Caption = 'Extract JPG'
        TabOrder = 4
        OnClick = btnExtractJPGClick
      end
    end
    object tsGenPDF: TTabSheet
      Caption = 'Generate PDF'
      ImageIndex = 1
      object lblOwnerPass: TLabel
        Left = 312
        Top = 64
        Width = 83
        Height = 13
        Caption = 'Owner Password:'
      end
      object lblUserPass: TLabel
        Left = 312
        Top = 128
        Width = 74
        Height = 13
        Caption = 'User Password:'
      end
      object btnPDFCreator: TButton
        Left = 200
        Top = 16
        Width = 89
        Height = 25
        Caption = 'Using Creator'
        TabOrder = 0
        OnClick = btnPDFCreatorClick
      end
      object btnImages: TButton
        Left = 104
        Top = 16
        Width = 89
        Height = 25
        Caption = 'Images To PDF'
        TabOrder = 1
        OnClick = btnImagesClick
      end
      object btnAddJPG: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Add JPGs'
        TabOrder = 2
        OnClick = btnAddJPGClick
      end
      object lstJpegs: TListBox
        Left = 16
        Top = 56
        Width = 273
        Height = 457
        ItemHeight = 13
        TabOrder = 3
      end
      object chkUsePass: TCheckBox
        Left = 320
        Top = 24
        Width = 129
        Height = 17
        Caption = 'With Encryption'
        TabOrder = 4
      end
      object edtOwnerPass: TEdit
        Left = 312
        Top = 88
        Width = 121
        Height = 21
        TabOrder = 5
        Text = '123456'
      end
      object edtUserPass: TEdit
        Left = 312
        Top = 152
        Width = 121
        Height = 21
        TabOrder = 6
        Text = '654321'
      end
    end
  end
  object dlgSave1: TSaveDialog
    Left = 448
    Top = 8
  end
  object dlgOpen1: TOpenDialog
    Left = 272
    Top = 16
  end
end
