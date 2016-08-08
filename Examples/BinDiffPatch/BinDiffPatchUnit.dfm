object BinaryDiffPatchForm: TBinaryDiffPatchForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Binary Diff & Patch Demo'
  ClientHeight = 315
  ClientWidth = 530
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
  object grpDiff: TGroupBox
    Left = 16
    Top = 16
    Width = 489
    Height = 265
    Caption = 'Binary Diff'
    TabOrder = 0
    object lblDiffOld: TLabel
      Left = 16
      Top = 32
      Width = 38
      Height = 13
      Caption = 'Old File:'
    end
    object lblDiffNew: TLabel
      Left = 16
      Top = 64
      Width = 44
      Height = 13
      Caption = 'New File:'
    end
    object lblPatch: TLabel
      Left = 16
      Top = 96
      Width = 50
      Height = 13
      Caption = 'Patch File:'
    end
    object lblPatchNew: TLabel
      Left = 16
      Top = 176
      Width = 44
      Height = 13
      Caption = 'New File:'
    end
    object edtDiffOld: TEdit
      Left = 72
      Top = 32
      Width = 297
      Height = 21
      TabOrder = 0
      Text = 'C:\1.c'
    end
    object edtDiffNew: TEdit
      Left = 72
      Top = 64
      Width = 297
      Height = 21
      TabOrder = 1
      Text = 'C:\2.c'
    end
    object btnDiffBrowseOld: TButton
      Left = 392
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 2
      OnClick = btnDiffBrowseOldClick
    end
    object btnDiffBrowseNew: TButton
      Left = 392
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 3
      OnClick = btnDiffBrowseNewClick
    end
    object btnDiff: TButton
      Left = 72
      Top = 136
      Width = 393
      Height = 25
      Caption = 'Binary Diff Old File and New File to Patch File'
      TabOrder = 4
      OnClick = btnDiffClick
    end
    object edtPatch: TEdit
      Left = 72
      Top = 96
      Width = 297
      Height = 21
      TabOrder = 5
      Text = 'C:\diff'
    end
    object btnDiffBrowsePatch: TButton
      Left = 392
      Top = 96
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 6
      OnClick = btnDiffBrowsePatchClick
    end
    object btnBinaryPatch: TButton
      Left = 72
      Top = 216
      Width = 393
      Height = 25
      Caption = 'Binary Patch Old File with Patch File to New File'
      TabOrder = 7
      OnClick = btnBinaryPatchClick
    end
    object edtPatchNew: TEdit
      Left = 72
      Top = 176
      Width = 297
      Height = 21
      TabOrder = 8
      Text = 'C:\2.c.new'
    end
    object btnPatchBrowseNew: TButton
      Left = 392
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 9
      OnClick = btnDiffBrowseNewClick
    end
  end
  object dlgOpen: TOpenDialog
    Left = 136
    Top = 64
  end
  object dlgSave: TSaveDialog
    FileName = 'diff.txt'
    Left = 280
    Top = 64
  end
end
