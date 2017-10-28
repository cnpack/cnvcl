object FormCalc: TFormCalc
  Left = 252
  Top = 157
  Width = 594
  Height = 425
  Caption = 'Simple Calculator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblExpression: TLabel
    Left = 16
    Top = 16
    Width = 54
    Height = 13
    Caption = 'Expression:'
  end
  object lblResult: TLabel
    Left = 16
    Top = 48
    Width = 33
    Height = 13
    Caption = 'Result:'
  end
  object lblExpr: TLabel
    Left = 80
    Top = 80
    Width = 3
    Height = 13
  end
  object edtExpr: TEdit
    Left = 80
    Top = 12
    Width = 481
    Height = 21
    TabOrder = 0
  end
  object edtResult: TEdit
    Left = 80
    Top = 44
    Width = 481
    Height = 21
    TabOrder = 1
  end
  object pnlButtons: TPanel
    Left = 16
    Top = 112
    Width = 545
    Height = 265
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object btnClear: TButton
      Left = 16
      Top = 16
      Width = 65
      Height = 25
      Caption = 'Clear'
      TabOrder = 0
      OnClick = btnClearClick
    end
    object btnBackSpace: TButton
      Left = 16
      Top = 56
      Width = 65
      Height = 25
      Caption = 'Backspace'
      TabOrder = 4
      OnClick = btnBackSpaceClick
    end
    object btn1: TButton
      Left = 16
      Top = 96
      Width = 65
      Height = 25
      Caption = '1'
      TabOrder = 7
      OnClick = btn1Click
    end
    object btn2: TButton
      Left = 96
      Top = 96
      Width = 65
      Height = 25
      Caption = '2'
      TabOrder = 8
      OnClick = btn1Click
    end
    object btn3: TButton
      Left = 176
      Top = 96
      Width = 65
      Height = 25
      Caption = '3'
      TabOrder = 9
      OnClick = btn1Click
    end
    object btn4: TButton
      Left = 16
      Top = 136
      Width = 65
      Height = 25
      Caption = '4'
      TabOrder = 10
      OnClick = btn1Click
    end
    object btn5: TButton
      Left = 96
      Top = 136
      Width = 65
      Height = 25
      Caption = '5'
      TabOrder = 11
      OnClick = btn1Click
    end
    object btn6: TButton
      Left = 176
      Top = 136
      Width = 65
      Height = 25
      Caption = '6'
      TabOrder = 12
      OnClick = btn1Click
    end
    object btn7: TButton
      Left = 16
      Top = 176
      Width = 65
      Height = 25
      Caption = '7'
      TabOrder = 13
      OnClick = btn1Click
    end
    object btn8: TButton
      Left = 96
      Top = 176
      Width = 65
      Height = 25
      Caption = '8'
      TabOrder = 14
      OnClick = btn1Click
    end
    object btn9: TButton
      Left = 176
      Top = 176
      Width = 65
      Height = 25
      Caption = '9'
      TabOrder = 15
      OnClick = btn1Click
    end
    object btn0: TButton
      Left = 96
      Top = 216
      Width = 65
      Height = 25
      Caption = '0'
      TabOrder = 17
      OnClick = btn1Click
    end
    object btnDot: TButton
      Left = 176
      Top = 216
      Width = 65
      Height = 25
      Caption = '.'
      TabOrder = 18
      OnClick = btn1Click
    end
    object btnRevert: TButton
      Left = 16
      Top = 216
      Width = 65
      Height = 25
      Caption = '+/-'
      TabOrder = 16
      OnClick = btnRevertClick
    end
    object btnAdd: TButton
      Left = 96
      Top = 16
      Width = 65
      Height = 25
      Caption = '+'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnSub: TButton
      Left = 176
      Top = 16
      Width = 65
      Height = 25
      Caption = '-'
      TabOrder = 2
      OnClick = btnAddClick
    end
    object btnMul: TButton
      Left = 96
      Top = 56
      Width = 65
      Height = 25
      Caption = '*'
      TabOrder = 5
      OnClick = btnAddClick
    end
    object btnDiv: TButton
      Left = 176
      Top = 56
      Width = 65
      Height = 25
      Caption = '/'
      TabOrder = 6
      OnClick = btnAddClick
    end
    object btnEnter: TButton
      Left = 280
      Top = 16
      Width = 241
      Height = 225
      Caption = 'Calculate!'
      TabOrder = 3
      OnClick = btnEnterClick
    end
  end
end
