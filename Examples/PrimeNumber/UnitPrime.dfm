object FormPrime: TFormPrime
  Left = 237
  Top = 121
  BorderStyle = bsDialog
  Caption = 'Prime Number Test'
  ClientHeight = 464
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 16
    Top = 16
    Width = 657
    Height = 429
    ActivePage = tsMontgomery
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsGenPrime: TTabSheet
      Caption = 'Find Prime Numbers'
      object btnGen: TButton
        Left = 16
        Top = 16
        Width = 209
        Height = 25
        Caption = 'Find Prime Numbers from 2 to'
        TabOrder = 0
        OnClick = btnGenClick
      end
      object edtMax: TEdit
        Left = 248
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '65537'
      end
      object mmoResult: TMemo
        Left = 16
        Top = 56
        Width = 617
        Height = 325
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object chkQuickGen: TCheckBox
        Left = 392
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Quick Mode'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object tsIsPrime: TTabSheet
      Caption = 'Is Prime'
      ImageIndex = 1
      object lblCheck: TLabel
        Left = 16
        Top = 28
        Width = 31
        Height = 13
        Caption = 'Check'
      end
      object lblInt64: TLabel
        Left = 16
        Top = 68
        Width = 31
        Height = 13
        Caption = 'Check'
      end
      object edtToPrime: TEdit
        Left = 64
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '39779'
      end
      object btnIsPrime: TButton
        Left = 208
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Is Prime£¿'
        TabOrder = 1
        OnClick = btnIsPrimeClick
      end
      object edtInt64: TEdit
        Left = 64
        Top = 64
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '397796406237767'
      end
      object btnInt64IsPrime: TButton
        Left = 208
        Top = 64
        Width = 105
        Height = 25
        Caption = 'Is Int64 Prime£¿'
        TabOrder = 3
        OnClick = btnInt64IsPrimeClick
      end
      object btnCarmichael: TButton
        Left = 336
        Top = 64
        Width = 297
        Height = 25
        Caption = 'Carmichael is Prime?'
        TabOrder = 4
        OnClick = btnCarmichaelClick
      end
      object mmoCar: TMemo
        Left = 16
        Top = 112
        Width = 617
        Height = 269
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
      object btnGen64: TButton
        Left = 336
        Top = 24
        Width = 145
        Height = 25
        Caption = 'Generate a Int64 Prime'
        TabOrder = 6
        OnClick = btnGen64Click
      end
    end
    object tsMontgomery: TTabSheet
      Caption = 'Montgomery Power Mod'
      ImageIndex = 2
      object lbl1: TLabel
        Left = 208
        Top = 24
        Width = 6
        Height = 13
        Caption = '^'
      end
      object lblMonMod: TLabel
        Left = 416
        Top = 28
        Width = 20
        Height = 13
        Caption = 'mod'
      end
      object btn2: TSpeedButton
        Left = 168
        Top = 48
        Width = 23
        Height = 22
        Caption = 'J'
        Flat = True
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Wingdings 3'
        Font.Style = []
        ParentFont = False
        OnClick = btn2Click
      end
      object btn65537: TSpeedButton
        Left = 224
        Top = 48
        Width = 63
        Height = 22
        Caption = '65537'
        Flat = True
        OnClick = btn65537Click
      end
      object edtMonA: TEdit
        Left = 24
        Top = 24
        Width = 177
        Height = 21
        TabOrder = 0
        Text = '12345678987654321'
      end
      object edtMonB: TEdit
        Left = 224
        Top = 24
        Width = 185
        Height = 21
        TabOrder = 1
        Text = '6405181853089073441'
      end
      object edtMonC: TEdit
        Left = 448
        Top = 24
        Width = 185
        Height = 21
        TabOrder = 2
        Text = '9676503616299152957'
      end
      object btnMon: TButton
        Left = 24
        Top = 72
        Width = 81
        Height = 21
        Caption = 'Power Mod'
        TabOrder = 3
        OnClick = btnMonClick
      end
      object edtMonRes: TEdit
        Left = 128
        Top = 72
        Width = 401
        Height = 21
        TabOrder = 4
      end
      object btnMonPowerMod64: TButton
        Left = 552
        Top = 72
        Width = 83
        Height = 21
        Caption = 'Power Mod U64'
        TabOrder = 5
        OnClick = btnMonPowerMod64Click
      end
    end
  end
end
