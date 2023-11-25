object FormOTS: TFormOTS
  Left = 192
  Top = 107
  Width = 1004
  Height = 578
  Caption = 'One Time Signature'
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
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 971
    Height = 524
    ActivePage = tsSM3OTS
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsSM3OTS: TTabSheet
      Caption = 'SM3 OTS'
      object lblSM3OTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSM3OTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSM3OTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSM3OTSVerifyKey: TLabel
        Left = 674
        Top = 323
        Width = 55
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Verification:'
      end
      object lblSM3OTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSM3OTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SM3 OTS Keys'
        TabOrder = 0
        OnClick = btnGenSM3OTSKeysClick
      end
      object mmoSM3OTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSM3OTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSM3OTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SM3 OTS.')
        TabOrder = 3
      end
      object btnSM3OTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 OTS Sign'
        TabOrder = 4
        OnClick = btnSM3OTSSignClick
      end
      object btnSM3OTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 OTS Verify'
        TabOrder = 5
        OnClick = btnSM3OTSVerifyClick
      end
      object mmoSM3OTSVerificationKey: TMemo
        Left = 674
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
      object mmoSM3OTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 7
        WordWrap = False
      end
    end
    object tsSHA256OTS: TTabSheet
      Caption = 'SHA256 OTS'
      ImageIndex = 1
      object lblSHA256OTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSHA256OTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSHA256OTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSHA256OTSVerifyKey: TLabel
        Left = 674
        Top = 323
        Width = 55
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Verification:'
      end
      object lblSHA256OTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSHA256OTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SHA256 OTS Keys'
        TabOrder = 0
        OnClick = btnGenSHA256OTSKeysClick
      end
      object mmoSHA256OTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSHA256OTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSHA256OTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SHA256 OTS.')
        TabOrder = 3
      end
      object btnSHA256OTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 OTS Sign'
        TabOrder = 4
        OnClick = btnSHA256OTSSignClick
      end
      object btnSHA256OTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 OTS Verify'
        TabOrder = 5
        OnClick = btnSHA256OTSVerifyClick
      end
      object mmoSHA256OTSVerificationKey: TMemo
        Left = 674
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
      object mmoSHA256OTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 7
        WordWrap = False
      end
    end
  end
end
