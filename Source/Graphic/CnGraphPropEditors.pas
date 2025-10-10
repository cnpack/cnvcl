{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGraphPropEditors;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�����ؼ������Ա༭����Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ��ǰ��Ϊ�ڲ��ο�������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.08 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  CnVCLBase, CnGraphics, CnImage, ExtDlgs;

type

{ TCnBitmapProperty }

  TCnBitmapProperty = class(TPropertyEditor)
  {* TBitmap���Ա༭���࣬����TCnImage�У��ڲ�������}
  public
    procedure Edit; override;
    {* �༭����}
    function GetAttributes: TPropertyAttributes; override;
    {* ȡ���Ա༭״̬}
    function GetValue: string; override;
    {* ȡ������ʾ���崮}
    procedure SetValue(const Value: string); override;
    {* ���������ı�ֵ}
  end;

implementation

{ TCnBitmapProperty }

procedure TCnBitmapProperty.Edit;
begin
  if GetComponent(0) is TCnImage then
    with TOpenPictureDialog.Create(nil) do
    try
      if Execute then
        TCnImage(GetComponent(0)).Bitmap.LoadFromFile(FileName);
    finally
      Free;
    end;
end;

function TCnBitmapProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TCnBitmapProperty.GetValue: string;
begin
  Result := '(None)';
  if GetComponent(0) is TCnImage then
    with TCnImage(GetComponent(0)) do
      if (Bitmap <> nil) and not Bitmap.Empty then
        Result := '(TCnBitmap)';
end;

procedure TCnBitmapProperty.SetValue(const Value: string);
begin
  if GetComponent(0) is TCnImage then
    if Value = '' then
      TCnImage(GetComponent(0)).Bitmap.FreeImage;
end;

end.



