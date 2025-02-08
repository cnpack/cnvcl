object FormNative: TFormNative
  Left = 236
  Top = 133
  BorderStyle = bsDialog
  Caption = 
    'Test some Native Declarations and Methods - Run on D567 and can ' +
    'be verified in BDS.'
  ClientHeight = 596
  ClientWidth = 954
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
    Width = 846
    Height = 473
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 18
  end
  object btnStrUInt64: TButton
    Left = 224
    Top = 16
    Width = 75
    Height = 25
    Caption = 'StrToUInt64'
    TabOrder = 2
    OnClick = btnStrUInt64Click
  end
  object btnMul32: TButton
    Left = 320
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Mul U32 to Int64'
    TabOrder = 3
    OnClick = btnMul32Click
  end
  object btnHighLowBits: TButton
    Left = 424
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Test High Low Bits'
    TabOrder = 4
    OnClick = btnHighLowBitsClick
  end
  object btnInt64MulMod: TButton
    Left = 536
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Int64 MulMod'
    TabOrder = 5
    OnClick = btnInt64MulModClick
  end
  object btnUInt64Add: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = 'UInt64 Add'
    TabOrder = 9
    OnClick = btnUInt64AddClick
  end
  object btnUInt64Mul: TButton
    Left = 112
    Top = 64
    Width = 75
    Height = 25
    Caption = 'UInt64 Mul'
    TabOrder = 10
    OnClick = btnUInt64MulClick
  end
  object btnGetGT2Power: TButton
    Left = 224
    Top = 64
    Width = 75
    Height = 25
    Caption = '> 2^n 32'
    TabOrder = 11
    OnClick = btnGetGT2PowerClick
  end
  object btnGetGT2Power64: TButton
    Left = 320
    Top = 64
    Width = 75
    Height = 25
    Caption = '> 2^n 64'
    TabOrder = 12
    OnClick = btnGetGT2Power64Click
  end
  object edtPower: TEdit
    Left = 416
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 13
    Text = '9'
  end
  object edtExponent: TEdit
    Left = 504
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 14
    Text = '10'
  end
  object edtUPower: TEdit
    Left = 600
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 15
    Text = '9'
  end
  object edtUExponent: TEdit
    Left = 688
    Top = 64
    Width = 49
    Height = 21
    TabOrder = 16
    Text = '10'
  end
  object btnInt64AddMod: TButton
    Left = 648
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Int64 AddMod'
    TabOrder = 6
    OnClick = btnInt64AddModClick
  end
  object btnEndian: TButton
    Left = 871
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Endian'
    TabOrder = 19
    OnClick = btnEndianClick
  end
  object chkSwap: TCheckBox
    Left = 879
    Top = 144
    Width = 65
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Swap'
    TabOrder = 20
  end
  object btnConstTimeCondSwap: TButton
    Left = 871
    Top = 168
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Const Time'
    TabOrder = 21
    OnClick = btnConstTimeCondSwapClick
  end
  object btnInt64DivMod: TButton
    Left = 871
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Int64 Div Mod'
    TabOrder = 22
    OnClick = btnInt64DivModClick
  end
  object btnUInt64DivMod: TButton
    Left = 871
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'UInt64 Div Mod'
    TabOrder = 23
    OnClick = btnUInt64DivModClick
  end
  object btnInt128DivMod: TButton
    Left = 871
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Int128 Div Mod'
    TabOrder = 24
    OnClick = btnInt128DivModClick
  end
  object btnUInt128DivMod: TButton
    Left = 871
    Top = 328
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'UInt128 Div Mod'
    TabOrder = 25
    OnClick = btnUInt128DivModClick
  end
  object btnToBinTest: TButton
    Left = 871
    Top = 368
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'To Binary Str'
    TabOrder = 26
    OnClick = btnToBinTestClick
  end
  object btnReverseBit: TButton
    Left = 872
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Reverse Bit'
    TabOrder = 27
    OnClick = btnReverseBitClick
  end
  object btn128Bit: TButton
    Left = 872
    Top = 448
    Width = 75
    Height = 25
    Caption = '128 Bit Test'
    TabOrder = 28
    OnClick = btn128BitClick
  end
  object btnU12864DivMod: TButton
    Left = 872
    Top = 488
    Width = 75
    Height = 25
    Caption = 'U128 64 D/M'
    TabOrder = 29
    OnClick = btnU12864DivModClick
  end
  object btn12864DivMod: TButton
    Left = 871
    Top = 528
    Width = 75
    Height = 25
    Caption = '128 64 D/M'
    TabOrder = 30
    OnClick = btn12864DivModClick
  end
  object btnMemSort: TButton
    Left = 872
    Top = 568
    Width = 75
    Height = 25
    Caption = 'Mem Sort'
    TabOrder = 31
    OnClick = btnMemSortClick
  end
  object btnUInt32ToStr: TButton
    Left = 872
    Top = 16
    Width = 75
    Height = 25
    Caption = 'UInt32 ToStr'
    TabOrder = 8
    OnClick = btnUInt32ToStrClick
  end
  object btnInt16ToLE: TButton
    Left = 872
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Int16 To LE'
    TabOrder = 17
    OnClick = btnInt16ToLEClick
  end
  object btn64SubOf32: TButton
    Left = 744
    Top = 16
    Width = 105
    Height = 25
    Caption = '64 Sub Overflow 32'
    TabOrder = 7
    OnClick = btn64SubOf32Click
  end
end
