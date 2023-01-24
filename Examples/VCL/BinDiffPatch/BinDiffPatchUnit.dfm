object BinaryDiffPatchForm: TBinaryDiffPatchForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Binary Diff & Patch Demo'
  ClientHeight = 376
  ClientWidth = 575
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
    Left = 16
    Top = 16
    Width = 537
    Height = 337
    ActivePage = tsDirectory
    TabOrder = 0
    object tsFile: TTabSheet
      Caption = 'File Diff/Patch'
      object grpDiff: TGroupBox
        Left = 16
        Top = 16
        Width = 489
        Height = 265
        Caption = 'Binary Diff/Patch'
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
          TabOrder = 2
          Text = 'C:\2.c'
        end
        object btnDiffBrowseOld: TButton
          Left = 392
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 1
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
          TabOrder = 6
          OnClick = btnDiffClick
        end
        object edtPatch: TEdit
          Left = 72
          Top = 96
          Width = 297
          Height = 21
          TabOrder = 4
          Text = 'C:\diff'
        end
        object btnDiffBrowsePatch: TButton
          Left = 392
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 5
          OnClick = btnDiffBrowsePatchClick
        end
        object btnBinaryPatch: TButton
          Left = 72
          Top = 216
          Width = 393
          Height = 25
          Caption = 'Binary Patch Old File with Patch File to New File'
          TabOrder = 9
          OnClick = btnBinaryPatchClick
        end
        object edtPatchNew: TEdit
          Left = 72
          Top = 176
          Width = 297
          Height = 21
          TabOrder = 7
          Text = 'C:\2.c.new'
        end
        object btnPatchBrowseNew: TButton
          Left = 392
          Top = 176
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 8
          OnClick = btnDiffBrowseNewClick
        end
      end
    end
    object tsDirectory: TTabSheet
      Caption = 'Directory Diff/Patch'
      ImageIndex = 1
      object grpDir: TGroupBox
        Left = 16
        Top = 16
        Width = 489
        Height = 273
        Caption = 'Directory Diff/Patch'
        TabOrder = 0
        object lblOldDir: TLabel
          Left = 16
          Top = 32
          Width = 35
          Height = 13
          Caption = 'Old Dir:'
        end
        object lblNewDir: TLabel
          Left = 16
          Top = 64
          Width = 41
          Height = 13
          Caption = 'New Dir:'
        end
        object lblPatchDir: TLabel
          Left = 16
          Top = 96
          Width = 47
          Height = 13
          Caption = 'Patch Dir:'
        end
        object lblOutDir: TLabel
          Left = 16
          Top = 180
          Width = 41
          Height = 13
          Caption = 'New Dir:'
        end
        object edtOldDir: TEdit
          Left = 72
          Top = 32
          Width = 297
          Height = 21
          TabOrder = 0
          Text = 'C:\1'
        end
        object edtNewDir: TEdit
          Left = 72
          Top = 64
          Width = 297
          Height = 21
          TabOrder = 2
          Text = 'C:\2'
        end
        object edtPatchDir: TEdit
          Left = 72
          Top = 96
          Width = 297
          Height = 21
          TabOrder = 4
          Text = 'C:\diffdir'
        end
        object btnBrowseOldDir: TButton
          Left = 392
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 1
          OnClick = btnBrowseOldDirClick
        end
        object btnBrowseNewDir: TButton
          Left = 392
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 3
          OnClick = btnBrowseNewDirClick
        end
        object btnBrowsePatchDir: TButton
          Left = 392
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 5
          OnClick = btnBrowsePatchDirClick
        end
        object btnDiffDir: TButton
          Left = 72
          Top = 136
          Width = 393
          Height = 25
          Caption = 'Binary Diff Old Directory and New Directory to Patch Directory'
          TabOrder = 6
          OnClick = btnDiffDirClick
        end
        object edtNewOutDir: TEdit
          Left = 72
          Top = 180
          Width = 297
          Height = 21
          TabOrder = 7
          Text = 'C:\newout'
        end
        object btnBrowseOutDir: TButton
          Left = 392
          Top = 180
          Width = 75
          Height = 25
          Caption = 'Browse'
          TabOrder = 8
          OnClick = btnBrowseOutDirClick
        end
        object btnDirPatch: TButton
          Left = 72
          Top = 216
          Width = 393
          Height = 25
          Caption = 'Binary Patch Old Directory with Patch Directory to New Directory'
          TabOrder = 9
          OnClick = btnDirPatchClick
        end
      end
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
