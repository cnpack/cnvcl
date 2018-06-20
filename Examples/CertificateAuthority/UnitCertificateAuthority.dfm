object FormCA: TFormCA
  Left = 354
  Top = 110
  Width = 705
  Height = 503
  Caption = 'Certificate Authority'
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
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 681
    Height = 457
    ActivePage = tsRequest
    TabOrder = 0
    object tsRequest: TTabSheet
      Caption = 'Certificate Sign Request'
      object grpGenRequest: TGroupBox
        Left = 8
        Top = 8
        Width = 657
        Height = 185
        Caption = 'Generate Certificate Sign Request'
        TabOrder = 0
        object lblKey: TLabel
          Left = 16
          Top = 24
          Width = 46
          Height = 13
          Caption = 'RSA Key:'
        end
        object lblContryName: TLabel
          Left = 16
          Top = 52
          Width = 64
          Height = 13
          Caption = 'Contry Name:'
        end
        object lblStateOrProvinceName: TLabel
          Left = 160
          Top = 52
          Width = 116
          Height = 13
          Caption = 'State or Province Name:'
        end
        object lblLocalityName: TLabel
          Left = 432
          Top = 52
          Width = 70
          Height = 13
          Caption = 'Locality Name:'
        end
        object lblOrgName: TLabel
          Left = 16
          Top = 84
          Width = 93
          Height = 13
          Caption = 'Organization Name:'
        end
        object lblOrgUnitName: TLabel
          Left = 304
          Top = 84
          Width = 123
          Height = 13
          Caption = 'Organizational Unit Name:'
        end
        object lblCommonName: TLabel
          Left = 16
          Top = 116
          Width = 75
          Height = 13
          Caption = 'Common Name:'
        end
        object lblEmail: TLabel
          Left = 304
          Top = 116
          Width = 69
          Height = 13
          Caption = 'Email Address:'
        end
        object lblHash: TLabel
          Left = 16
          Top = 148
          Width = 74
          Height = 13
          Caption = 'Hash Algorithm:'
        end
        object edtRSAKey: TEdit
          Left = 88
          Top = 20
          Width = 457
          Height = 21
          TabOrder = 0
        end
        object btnBrowseKey: TButton
          Left = 552
          Top = 20
          Width = 91
          Height = 21
          Caption = 'Browse Key File'
          TabOrder = 1
        end
        object edtContryName: TEdit
          Left = 88
          Top = 48
          Width = 49
          Height = 21
          TabOrder = 2
          Text = 'CN'
        end
        object edtStateOrProvinceName: TEdit
          Left = 288
          Top = 48
          Width = 121
          Height = 21
          TabOrder = 3
          Text = 'ZheJiang'
        end
        object edtLocalityName: TEdit
          Left = 512
          Top = 48
          Width = 129
          Height = 21
          TabOrder = 4
          Text = 'HangZhou'
        end
        object edtOrgName: TEdit
          Left = 120
          Top = 80
          Width = 161
          Height = 21
          TabOrder = 5
          Text = 'CnPack'
        end
        object edtOrgUnitName: TEdit
          Left = 440
          Top = 80
          Width = 201
          Height = 21
          TabOrder = 6
          Text = 'CnPack Team'
        end
        object edtCommonName: TEdit
          Left = 120
          Top = 112
          Width = 161
          Height = 21
          TabOrder = 7
          Text = 'cnpack.org'
        end
        object edtEmail: TEdit
          Left = 440
          Top = 112
          Width = 201
          Height = 21
          TabOrder = 8
          Text = 'master@cnpack.org'
        end
        object cbbHash: TComboBox
          Left = 120
          Top = 144
          Width = 161
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 9
          Items.Strings = (
            'MD5'
            'SHA1'
            'SHA256')
        end
        object btnGenerateCSR: TButton
          Left = 304
          Top = 144
          Width = 209
          Height = 21
          Caption = 'Generate Certificate Sign Request'
          TabOrder = 10
        end
        object btnSelfSign: TButton
          Left = 528
          Top = 144
          Width = 113
          Height = 21
          Caption = 'Self Sign'
          TabOrder = 11
        end
      end
      object grpParse: TGroupBox
        Left = 8
        Top = 200
        Width = 657
        Height = 209
        Caption = 'Parse CSR File'
        TabOrder = 1
        object lblCSR: TLabel
          Left = 16
          Top = 24
          Width = 44
          Height = 13
          Caption = 'CSR File:'
        end
        object edtCSR: TEdit
          Left = 88
          Top = 20
          Width = 457
          Height = 21
          TabOrder = 0
        end
        object btnBrowseCSR: TButton
          Left = 552
          Top = 20
          Width = 91
          Height = 21
          Caption = 'Browse CSR File'
          TabOrder = 1
        end
        object mmoCSRParse: TMemo
          Left = 16
          Top = 56
          Width = 625
          Height = 137
          TabOrder = 2
        end
      end
    end
    object tsSign: TTabSheet
      Caption = 'Sign Certificate'
      ImageIndex = 1
      object grpSign: TGroupBox
        Left = 8
        Top = 8
        Width = 657
        Height = 153
        Caption = 'Sign CSR File'
        TabOrder = 0
        object lblSignCSR: TLabel
          Left = 16
          Top = 24
          Width = 44
          Height = 13
          Caption = 'CSR File:'
        end
        object lblRoot: TLabel
          Left = 16
          Top = 56
          Width = 47
          Height = 13
          Caption = 'Root Key:'
        end
        object lblRootCrt: TLabel
          Left = 16
          Top = 88
          Width = 51
          Height = 13
          Caption = 'Root CRT:'
        end
        object edtSignCSR: TEdit
          Left = 88
          Top = 20
          Width = 457
          Height = 21
          TabOrder = 0
        end
        object btnSignCSRBrowse: TButton
          Left = 552
          Top = 20
          Width = 91
          Height = 21
          Caption = 'Browse CSR File'
          TabOrder = 1
        end
        object edtSignKey: TEdit
          Left = 88
          Top = 52
          Width = 457
          Height = 21
          TabOrder = 2
        end
        object btnSignKeyBrowse: TButton
          Left = 552
          Top = 52
          Width = 91
          Height = 21
          Caption = 'Browse Key File'
          TabOrder = 3
        end
        object btnSign: TButton
          Left = 16
          Top = 118
          Width = 625
          Height = 21
          Caption = 'Sign CSR File to CRT File '
          TabOrder = 4
        end
        object edtRootCRT: TEdit
          Left = 88
          Top = 84
          Width = 457
          Height = 21
          TabOrder = 5
        end
        object btnRootCRTBrowse: TButton
          Left = 552
          Top = 84
          Width = 91
          Height = 21
          Caption = 'Browse CRT File'
          TabOrder = 6
        end
      end
      object grpParseCER: TGroupBox
        Left = 8
        Top = 168
        Width = 657
        Height = 249
        Caption = 'Parse CER File'
        TabOrder = 1
        object lblCER: TLabel
          Left = 16
          Top = 24
          Width = 44
          Height = 13
          Caption = 'CER File:'
        end
        object edtCER: TEdit
          Left = 88
          Top = 20
          Width = 457
          Height = 21
          TabOrder = 0
        end
        object btnBrowseCER: TButton
          Left = 552
          Top = 20
          Width = 91
          Height = 21
          Caption = 'Browse CER File'
          TabOrder = 1
        end
        object mmoCER: TMemo
          Left = 16
          Top = 56
          Width = 625
          Height = 177
          TabOrder = 2
        end
      end
    end
  end
  object dlgOpen: TOpenDialog
    Left = 532
    Top = 56
  end
  object dlgSaveCSR: TSaveDialog
    Left = 412
    Top = 152
  end
end
