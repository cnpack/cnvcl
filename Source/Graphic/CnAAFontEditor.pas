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

unit CnAAFontEditor;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �ؼ���
* ��Ԫ���ƣ�ƽ����Ч�������ԡ�����༭����Ԫ
* ��Ԫ���ߣ�CnPack ������ �ܾ��� (zjy@cnpack.org)
*           ��ֲ��e- 
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Build 5/6
* ��    ע��
* �����£�2002.07.02
* ��ֲ���ڣ�2006.08.18
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes,
{$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}

type

{ TCnAAEffectProperty }

  TCnAAEffectProperty = class(TClassProperty)
  {* TAAEffectƽ��������Ч���������Ա༭����ͨ�����û���������ڿ��ӻ��༭ƽ��
     ������Ч������}
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

{ TCnAALabelEditor }

  TCnAALabelEditor = class(TComponentEditor)
  {* TAALabel���������ؼ�������༭����ͨ�����û���������ڿ��ӻ��༭ƽ��
     ������Ч������}
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  CnAAFont, CnAACtrls, CnAAFontDialog;

resourcestring
  SAAEffect = 'ƽ����Ч����';
  SAALabelVert = '����ƽ����Ч����(&F)...';

{ TCnAAEffectProperty }

type
  TCnAAEffectAccess = class(TCnAAEffect);

procedure TCnAAEffectProperty.Edit;
var
  AEffect: TCnAAEffect;
  FontLabel: TCnFontLabel;
begin
  AEffect := TCnAAEffect(Pointer(GetOrdValue));
  with TCnAAFontDialog.Create(nil) do
  try
    if (PropCount = 1) and (TCnAAEffectAccess(AEffect).GetOwner is TCnFontLabel) then
    begin
      FontLabel := TCnFontLabel(TCnAAEffectAccess(AEffect).GetOwner);
      AllowChangeFont := True;
      Font.Assign(FontLabel.Font);
    end
    else
    begin
      FontLabel := nil;
      AllowChangeFont := False;
    end;
    Effect.Assign(AEffect);
    if Execute then
    begin
      SetOrdValue(Integer(Effect));
      if FontLabel <> nil then
        FontLabel.Font.Assign(Font);
    end;
  finally
    Free;
  end;
end;

function TCnAAEffectProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly, paMultiSelect];
end;

function TCnAAEffectProperty.GetValue: string;
begin
  Result := SAAEffect;
end;

{ TCnAALabelEditor }

procedure TCnAALabelEditor.ExecuteVerb(Index: Integer);
var
  Ctrl: TCnAALabel;
begin
  if Index = 0 then
  begin
    if Component is TCnAALabel then
    begin
      Ctrl := TCnAALabel(Component);
      with TCnAAFontDialog.Create(nil) do
      try
        AllowChangeFont := True;
        Font.Assign(Ctrl.Font);
        Effect.Assign(Ctrl.Effect.FontEffect);
        if Execute then
        begin
          Ctrl.Font.Assign(Font);
          Ctrl.Effect.FontEffect := Effect;
          Designer.Modified;
        end;
      finally
        Free;
      end;
    end;
  end
  else
    inherited;
end;

function TCnAALabelEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := SAALabelVert
  else
    Result := inherited GetVerb(Index);
end;

function TCnAALabelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.



