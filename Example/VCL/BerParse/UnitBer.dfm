object FormParseBer: TFormParseBer
  Left = 201
  Top = 95
  Width = 832
  Height = 486
  Caption = 'BER Format Reader & Writer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblBin: TLabel
    Left = 8
    Top = 24
    Width = 45
    Height = 13
    Caption = 'PEM File:'
  end
  object mmoResult: TMemo
    Left = 8
    Top = 56
    Width = 337
    Height = 351
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    TabOrder = 0
  end
  object btnParse: TButton
    Left = 723
    Top = 20
    Width = 83
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Binary Parse'
    TabOrder = 1
    OnClick = btnParseClick
  end
  object tv1: TTreeView
    Left = 352
    Top = 56
    Width = 452
    Height = 351
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoExpand = True
    Indent = 19
    PopupMenu = pmTree
    TabOrder = 2
    OnCollapsing = tv1Collapsing
    OnDblClick = tv1DblClick
  end
  object edtFile: TEdit
    Left = 64
    Top = 20
    Width = 305
    Height = 21
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 376
    Top = 20
    Width = 75
    Height = 21
    Caption = 'Browse'
    TabOrder = 4
    OnClick = btnBrowseClick
  end
  object btnWrite: TButton
    Left = 264
    Top = 424
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 5
    OnClick = btnWriteClick
  end
  object btnDeBase64Parse: TButton
    Left = 619
    Top = 20
    Width = 91
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Base64 Parse'
    TabOrder = 6
    OnClick = btnDeBase64ParseClick
  end
  object chkParseInner: TCheckBox
    Left = 496
    Top = 22
    Width = 113
    Height = 17
    Caption = 'Parse Inner String'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object dlgOpen: TOpenDialog
    Left = 336
    Top = 16
  end
  object dlgSave: TSaveDialog
    Left = 360
    Top = 392
  end
  object pmTree: TPopupMenu
    Left = 544
    Top = 256
    object ShowContent1: TMenuItem
      Caption = 'Show Content'
      Default = True
      OnClick = ShowContent1Click
    end
    object SaveNodeContent1: TMenuItem
      Caption = 'Save Node Data...'
      OnClick = SaveNodeContent1Click
    end
    object SaveNodeContent2: TMenuItem
      Caption = 'Save Node Content...'
      OnClick = SaveNodeContent2Click
    end
  end
end
