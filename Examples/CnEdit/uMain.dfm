object frmMain: TfrmMain
  Left = 208
  Top = 170
  Width = 273
  Height = 200
  Caption = 'CnEditDemo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblNormalText: TLabel
    Left = 8
    Top = 8
    Width = 57
    Height = 13
    Caption = 'NormalText:'
  end
  object lblIntegerText: TLabel
    Left = 8
    Top = 48
    Width = 57
    Height = 13
    Caption = 'IntegerText:'
  end
  object lblFloatText: TLabel
    Left = 8
    Top = 88
    Width = 47
    Height = 13
    Caption = 'FloatText:'
  end
  object lblIdentText: TLabel
    Left = 8
    Top = 128
    Width = 48
    Height = 13
    Caption = 'IdentText:'
  end
  object lblIntegerText2: TLabel
    Left = 136
    Top = 48
    Width = 81
    Height = 13
    Caption = 'IntegerText:(>=0)'
  end
  object lblFloatText2: TLabel
    Left = 136
    Top = 88
    Width = 71
    Height = 13
    Caption = 'FloatText:(>=0)'
  end
  object lbl1: TLabel
    Left = 136
    Top = 8
    Width = 59
    Height = 13
    Caption = 'With Button:'
  end
  object edtNormalText: TCnEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    OnExit = EditorExit
    AcceptCharList = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'
  end
  object edtIntegerText: TCnEdit
    Left = 8
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 1
    OnExit = EditorExit
    TextType = IntegerText
  end
  object edtFloatText: TCnEdit
    Left = 8
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 2
    OnExit = EditorExit
    TextType = FloatText
  end
  object edtIdentText: TCnEdit
    Left = 8
    Top = 144
    Width = 121
    Height = 21
    TabOrder = 3
    OnExit = EditorExit
    TextType = IdentText
  end
  object edtIntegerText2: TCnEdit
    Left = 136
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 4
    OnExit = EditorExit
    TextType = IntegerText
    AcceptNegative = False
  end
  object edtFloatText2: TCnEdit
    Left = 136
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 5
    OnExit = EditorExit
    TextType = FloatText
    AcceptNegative = False
  end
  object btnGetValue: TButton
    Left = 136
    Top = 144
    Width = 75
    Height = 25
    Caption = 'GetValue'
    TabOrder = 6
    OnClick = btnGetValueClick
  end
  object edt1: TCnEdit
    Left = 136
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 7
    OnExit = EditorExit
    OnButtonClick = edt1ButtonClick
    LinkStyle = lsEllipsis
    ButtonCursor = crDrag
    TextType = IdentText
    AcceptCharList = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'
  end
end
