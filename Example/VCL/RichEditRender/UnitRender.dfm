object FormRender: TFormRender
  Left = 67
  Top = 56
  Width = 1331
  Height = 633
  Caption = 'RichEdit Render'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 440
    Top = 40
    Width = 857
    Height = 529
  end
  object Bevel1: TBevel
    Left = 440
    Top = 16
    Width = 400
    Height = 10
    Shape = bsBottomLine
  end
  object lblWidth: TLabel
    Left = 224
    Top = 68
    Width = 129
    Height = 13
    Caption = '宽度（0为不限制）：'
  end
  object bvl1: TBevel
    Left = 840
    Top = 16
    Width = 400
    Height = 10
    Shape = bsTopLine
  end
  object lblColor: TLabel
    Left = 224
    Top = 44
    Width = 48
    Height = 13
    Caption = '背景色：'
  end
  object shp1: TShape
    Left = 352
    Top = 40
    Width = 17
    Height = 17
    OnMouseDown = shp1MouseDown
  end
  object btnTestRender: TButton
    Left = 224
    Top = 8
    Width = 193
    Height = 25
    Caption = 'Test Render'
    TabOrder = 0
    OnClick = btnTestRenderClick
  end
  object redt1: TRichEdit
    Left = 24
    Top = 96
    Width = 401
    Height = 473
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'redt1')
    ParentFont = False
    TabOrder = 1
    WordWrap = False
  end
  object btnTestLoad: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Test Load'
    TabOrder = 2
    OnClick = btnTestLoadClick
  end
  object btnTestSetText: TButton
    Left = 24
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Test SetText'
    TabOrder = 3
    OnClick = btnTestSetTextClick
  end
  object edtWidth: TEdit
    Left = 352
    Top = 64
    Width = 65
    Height = 21
    TabOrder = 4
    Text = '0'
  end
  object dlgOpen1: TOpenDialog
    Left = 40
    Top = 80
  end
  object dlgColor1: TColorDialog
    Ctl3D = True
    Left = 304
    Top = 40
  end
end
