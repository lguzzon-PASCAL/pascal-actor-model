{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 2012 - J. Aldo G. de Freitas Junior

{$mode objfpc}
{$H+}{$M+}

Unit
	RTTIThreads;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	TypInfo,
	Variants;

Type
	TRTTIThread = Class(TThread)
	Private
		fTypeData : PTypeData;
		fPropList : PPropList;
		fPropCount : Integer;
		Function GetPropertyIndex(Const aName : String) : Integer;
		Function GetPropertyType(Const aIndex : Integer) : TTypeKind;
		Procedure InitRTTI;
		Procedure DoneRTTI;
	Public
		Procedure DispatchEvent(Const aEventName : String; Const aObject : TObject);
		Function GetPropertyValueByName(Const aName : String) : Variant;
		Procedure SetPropertyValueByName(Const aName : String; Const aValue : Variant);
		Constructor Create(Const aCreateSuspended : Boolean = True; Const aStackSize : SizeUInt = DefaultStackSize);
		Destructor Destroy; Override;
	End;

Implementation

Const
	tiStreamable : Set Of TTypeKind = [
		tkInteger,
		tkChar,
		tkEnumeration,
		tkFloat,
		tkSet,
		tkSString,
		tkLString,
		tkAString,
		tkWString,
		tkVariant,
		tkWChar,
		tkBool,
		tkInt64,
		tkQWord
	];

Type
	TRTTIEvent = Record
		MsgStr : String[255];
		Data : Pointer;
	End;

Function TRTTIThread.GetPropertyIndex(Const aName : String) : Integer;
Var
	lRow : Integer;
Begin
	Result := -1;
	For lRow := 0 To fPropCount - 1 Do
		If LowerCase(fPropList^[lRow]^.Name) = LowerCase(aName) Then
		Begin
			Result := lRow;
			Break;
		End;
End;

Function TRTTIThread.GetPropertyType(Const aIndex : Integer) : TTypeKind;
Begin
	Result := fPropList^[aIndex]^.PropType^.Kind;
End;

Procedure TRTTIThread.InitRTTI;
Begin
	fTypeData := GetTypeData(Self.ClassInfo);
	GetMem(fPropList, fTypeData^.PropCount * SizeOf(Pointer));
	fPropCount := GetPropList(Self.ClassInfo, tiStreamable, fPropList);
End;

Procedure TRTTIThread.DoneRTTI;
Begin
	FreeMem(fPropList, fTypeData^.PropCount * SizeOf(Pointer));
End;

Procedure TRTTIThread.DispatchEvent(Const aEventName : String; Const aObject : TObject);
Var
	lEvent : TRTTIEvent;
Begin
	lEvent.MsgStr := aEventName;
	lEvent.Data := Pointer(aObject);
	// Debug WriteLn('Assigned : ', Assigned(Self));
	DispatchStr(lEvent);
End;

Function TRTTIThread.GetPropertyValueByName(Const aName : String) : Variant;
Begin
	Case GetPropertyType(GetPropertyIndex(aName)) Of
		tkInteger     : Result := GetInt64Prop(Self, aName);
		tkChar        : Result := GetStrProp(Self, aName);
		tkEnumeration : Result := GetEnumProp(Self, aName);
		tkFloat       : Result := GetFloatProp(Self, aName);
		tkSet         : Result := GetSetProp(Self, aName, True);
		tkSString     : Result := GetStrProp(Self, aName);
		tkLString     : Result := GetStrProp(Self, aName);
		tkAString     : Result := GetStrProp(Self, aName);
		tkWString     : Result := GetWideStrProp(Self, aName);
		tkVariant     : Result := GetVariantProp(Self, aName);
		tkWChar       : Result := GetWideStrProp(Self, aName);
		tkBool        : Result := GetEnumProp(Self, aName);
		tkInt64       : Result := GetInt64Prop(Self, aName);
		tkQWord       : Result := GetInt64Prop(Self, aName);
	End;
End;

Procedure TRTTIThread.SetPropertyValueByName(Const aName : String; Const aValue : Variant);
Begin
	Case GetPropertyType(GetPropertyIndex(aName)) Of
		tkInteger     : SetOrdProp(Self, aName, aValue);
		tkChar        : SetStrProp(Self, aName, aValue);
		tkEnumeration : SetEnumProp(Self, aName, aValue);
		tkFloat       : SetFloatProp(Self, aName, aValue);
		tkSet         : SetSetProp(Self, aName, aValue);
		tkSString     : SetStrProp(Self, aName, aValue);
		tkLString     : SetStrProp(Self, aName, aValue);
		tkAString     : SetStrProp(Self, aName, aValue);
		tkWString     : SetStrProp(Self, aName, aValue);
		tkVariant     : SetVariantProp(Self, aName, aValue);
		tkWChar       : SetWideStrProp(Self, aName, aValue);
		tkBool        : SetEnumProp(Self, aName, aValue);
		tkInt64       : SetInt64Prop(Self, aName, aValue);
		tkQWord       : SetInt64Prop(Self, aName, aValue);
	End;
End;

Constructor TRTTIThread.Create(Const aCreateSuspended : Boolean = True; Const aStackSize : SizeUInt = DefaultStackSize);
Begin
	Inherited Create(aCreateSuspended, aStackSize);
	InitRTTI;
End;

Destructor TRTTIThread.Destroy;
Begin
	DoneRTTI;
	Inherited Destroy;
End;

End.