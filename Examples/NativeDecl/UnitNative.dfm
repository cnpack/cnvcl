object FormNative: TFormNative
  Left = 236
  Top = 153
  BorderStyle = bsDialog
  Caption = 
    'Test some Native Declarations and Methods - Run on D567 and can ' +
    'be verified in BDS.'
  ClientHeight = 477
  ClientWidth = 747
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bvl1: TBevel
    Left = 208
    Top = 8
    Width = 17
    Height = 41
    Shape = bsLeftLine
  end
  object btnPower: TSpeedButton
    Left = 472
    Top = 56
    Width = 23
    Height = 22
    Caption = '^'
    OnClick = btnPowerClick
  end
  object btnRoot: TSpeedButton
    Left = 472
    Top = 80
    Width = 23
    Height = 22
    Caption = 'root'
    OnClick = btnRootClick
  end
  object btnURoot: TSpeedButton
    Left = 656
    Top = 64
    Width = 23
    Height = 22
    Caption = 'root'
    OnClick = btnURootClick
  end
  object bvl2: TBevel
    Left = 208
    Top = 56
    Width = 17
    Height = 41
    Shape = bsLeftLine
  end
  object bvl3: TBevel
    Left = 576
    Top = 56
    Width = 17
    Height = 41
    Shape = bsLeftLine
  end
  object btnUInt64Div: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'UInt64 Div'
    TabOrder = 0
    OnClick = btnUInt64DivClick
  end
  object btnUInt64Mod: TButton
    Left = 112
    Top = 16
    Width = 75
    Height = 25
    Caption = 'UInt64 Mod'
    TabOrder = 1
    OnClick = btnUInt64ModClick
  end
  object mmoRes: TMemo
    Left = 16
    Top = 104
    Width = 639
    Height = 354
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object btnStrUInt64: TButton
    Left = 224
    Top = 16
    Width = 75
    Height = 25
    Caption = 'StrToUInt64'
    TabOrder = 3
    OnClick = btnStrUInt64Click
  end
  object btnMul32: TButton
    Left = 320
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Mul U32 to Int64'
    TabOrder = 4
    OnClick = btnMul32Click
  end
  object btnHighLowBits: TButton
    Left = 424
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Test High Low Bits'
    TabOrder = 5
    OnClick = btnHighLowBitsClick
  end
  object btnInt64MulMod: TButton
    Left = 536
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Int64 MulMod'
    TabOrder = 6
    OnClick = btnInt64MulModClick
  end
  object btnUInt64Add: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = 'UInt64 Add'
    TabOrder = 7
    OnClick = btnUInt64AddClick
  end
  object btnUInt64Mul: TButton
    Left = 112
    Top = 64
    Width = 75
    Height = 25
    Caption = 'UInt64 Mul'
    TabOrder = 8
    OnClick = btnUInt64MulClick
  end
  object btnGetGT2Power: TButton
    Left = 224
    Top = 64
    Width = 75
    Height = 25
    Caption = '> 2^n 32'
    TabOrder = 9
    OnClick = btnGetGT2PowerClick
  end
  object btnGetGT2Power64: TButton
    Left = 320
    Top = 64
    Width = 75
    Height = 25
    Caption = '> 2^n 64'
    TabOrder = 10
    OnClick = btnGetGT2Power64Click
  end
  object edtPower: TEdit
    Left = 416
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 11
    Text = '9'
  end
  object edtExponent: TEdit
    Left = 504
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 12
    Text = '10'
  end
  object edtUPower: TEdit
    Left = 600
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 13
    Text = '9'
  end
  object edtUExponent: TEdit
    Left = 688
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 14
    Text = '10'
  end
  object btnInt64AddMod: TButton
    Left = 648
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Int64 AddMod'
    TabOrder = 15
    OnClick = btnInt64AddModClick
  end
  object btnEndian: TButton
    Left = 664
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Endian'
    TabOrder = 16
    OnClick = btnEndianClick
  end
  object chkSwap: TCheckBox
    Left = 672
    Top = 144
    Width = 65
    Height = 17
    Caption = 'Swap'
    TabOrder = 17
  end
  object btnConstTimeCondSwap: TButton
    Left = 664
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Const Time'
    TabOrder = 18
    OnClick = btnConstTimeCondSwapClick
  end
end
