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
	RTTIObjects;

Interface

Uses
	Classes,
	Contnrs,
	StrUtils,
	SysUtils,
	TypInfo,
	Variants;

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
	TArrayOfString = Array Of String;

	TRTTIEvent = Record
		MsgStr : String[255];
		Data : Pointer;
	End;

	ERTTIObject = Class(Exception);
	TRTTIObject = Class(TObject)
	Private
		fTypeData : PTypeData;
		fPropList : PPropList;
		fCount : Integer;
		fReadOnly : Boolean;
		Function GetPropertyValueByIndex(Const aIndex : Integer) : Variant;
		Procedure SetPropertyValueByIndex(Const aIndex : Integer; Const aValue : Variant);
		Function GetPropertyValueByName(Const aName : String) : Variant;
		Procedure SetPropertyValueByName(Const aName : String; Const aValue : Variant);
	Public
		Function GetPropertyIndex(Const aName : String) : Integer;
		Function GetPropertyName(Const aIndex : Integer) : String;
		Function GetPropertyType(Const aIndex : Integer) : TTypeKind;
		Function GetPropertyTypeName(Const aIndex : Integer) : String;
		Function IsObjectProperty(Const aIndex : Integer) : Boolean;
		Function GetEnumPropertyPossibleValues(Const aName : String): TArrayOfString;
		Function HasProperty(Const aName : String): Boolean;
		Procedure InitRTTI;
		Procedure DoneRTTI;
		Procedure InitRTTIFromStream(Const aStream : TStream); Virtual;
		Procedure SaveRTTIToStream(Const aStream : TStream); Virtual;
		Procedure InitRTTIFromString(Const aString : String); Virtual;
		Function SaveRTTIToString: String; Virtual;
		Function FormattedProperties: String;
		Procedure DispatchEvent(Const aEventName : String; Const aObject : TObject);
		// Properties
		Property Properties[aName : String] : Variant Read GetPropertyValueByName Write SetPropertyValueByName;
		Property PropertiesByIndex[aIndex : Integer] : Variant Read GetPropertyValueByIndex Write SetPropertyValueByIndex;
		Property PropertyCount : Integer Read fCount;
		Property ReadOnly : Boolean Read fReadOnly Write fReadOnly;
	End;

Function GetClassFromStream(Const aStream : TStream): String;
Function GetClassFromString(Const aString : String): String;

Implementation

// TRTTIObject

Function TRTTIObject.GetPropertyValueByIndex(Const aIndex : Integer) : Variant;
Begin
	If (aIndex >= 0) And (aIndex < PropertyCount) Then
		Result := GetPropertyValueByName(GetPropertyName(aIndex))
	Else
		Raise ERTTIObject.Create('No such property index.');
End;

Procedure TRTTIObject.SetPropertyValueByIndex(Const aIndex : Integer; Const aValue : Variant);
Begin
	If Not fReadOnly Then
		If (aIndex >= 0) And (aIndex < PropertyCount) Then
			SetPropertyValueByName(GetPropertyName(aIndex), aValue)
		Else
			Raise ERTTIObject.Create('No such property index.')
	Else
		Raise ERTTIOBject.Create('Read only object.');
End;

Function TRTTIObject.GetPropertyValueByName(Const aName : String) : Variant;
Var
	lIndex : Integer;
Begin
	lIndex := GetPropertyIndex(aName);
	If (lIndex >= 0) And (lIndex < PropertyCount) Then
	Begin
		Case GetPropertyType(lIndex) Of
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
	End
	Else
		Raise ERTTIObject.Create('No such property ' + aName);
End;

Procedure TRTTIObject.SetPropertyValueByName(Const aName : String; Const aValue : Variant);
Var
	lIndex : Integer;
Begin
	If Not fReadOnly Then
	Begin
		lIndex := GetPropertyIndex(aName);
		If (lIndex >= 0) And (lIndex < PropertyCount) Then
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
		End
		Else
			Raise ERTTIObject.Create('No such property ' + aName);
	End
	Else
		Raise ERTTIObject.Create('Cannot set property, the object is read-only.');
End;

Function TRTTIObject.GetPropertyIndex(Const aName : String) : Integer;
Var
	lRow : Integer;
Begin
	Result := -1;
	For lRow := 0 To fCount - 1 Do
		If LowerCase(fPropList^[lRow]^.Name) = LowerCase(aName) Then
		Begin
			Result := lRow;
			Break;
		End;
End;

Function TRTTIObject.GetPropertyName(Const aIndex : Integer) : String;
Begin
	Result := fPropList^[aIndex]^.Name;
End;

Function TRTTIObject.GetPropertyType(Const aIndex : Integer) : TTypeKind;
Begin
	Result := fPropList^[aIndex]^.PropType^.Kind;
End;

Function TRTTIObject.GetPropertyTypeName(Const aIndex : Integer) : String;
Begin
	Result := fPropList^[aIndex]^.PropType^.Name;
End;

Function TRTTIObject.IsObjectProperty(Const aIndex : Integer) : Boolean;
Begin
	Result := fPropList^[aIndex]^.PropType^.Kind = tkClass;
End;

Function TRTTIObject.GetEnumPropertyPossibleValues(Const aName : String): TArrayOfString;
Var
	lCtrl : Integer;
Begin
	SetLength(Result, 0);
	For lCtrl := 0 To GetEnumNameCount(FindPropInfo(Self, aName)^.PropType) Do
	Begin
		SetLength(Result, Length(Result) + 1);
		Result[High(Result)] := GetEnumName(FindPropInfo(Self, aName)^.PropType, lCtrl);
	End;
End;

Function TRTTIObject.HasProperty(Const aName : String): Boolean;
Begin
	Result := GetPropertyIndex(aName) >= 0;
End;

Procedure TRTTIObject.InitRTTI;
Begin
	fTypeData := GetTypeData(Self.ClassInfo);
	GetMem(fPropList, fTypeData^.PropCount * SizeOf(Pointer));
	fCount := GetPropList(Self.ClassInfo, tiStreamable, fPropList);
End;

Procedure TRTTIObject.DoneRTTI;
Begin
	FreeMem(fPropList, fTypeData^.PropCount * SizeOf(Pointer));
End;

Procedure TRTTIObject.InitRTTIFromStream(Const aStream : TStream);

	Function ReadInteger: Integer;
	Begin
		aStream.ReadBuffer(Result, SizeOf(Integer));
	End;

	Function ReadChar: Char;
	Begin
		aStream.ReadBuffer(Result, SizeOf(Char));
	End;

	Function ReadFloat: Extended;
	Begin
		aStream.ReadBuffer(Result, SizeOf(Extended));
	End;

	Function ReadString: String;
	Begin
		Result := aStream.ReadAnsiString;
	End;

	Function ReadBoolean: Integer;
	Begin
		aStream.ReadBuffer(Result, SizeOf(Boolean));
	End;

	Function ReadInt64: Int64;
	Begin
		aStream.ReadBuffer(Result, SizeOf(Int64));
	End;

Var
	lCtrl : Integer;

Begin
	// Carefull: The stream must be on the correct spot.
	For lCtrl := 0 To PropertyCount - 1 Do
		Case GetPropertyType(lCtrl) Of
			tkInteger     : SetOrdProp(Self, GetPropertyName(lCtrl), ReadInteger);
			tkChar        : SetStrProp(Self, GetPropertyName(lCtrl), ReadChar);
			tkEnumeration : SetOrdProp(Self, GetPropertyName(lCtrl), ReadInteger);
			tkFloat       : SetFloatProp(Self, GetPropertyName(lCtrl), ReadFloat);
			tkSet         : SetOrdProp(Self, GetPropertyName(lCtrl), ReadInteger);
			tkSString     : SetStrProp(Self, GetPropertyName(lCtrl), ReadString);
			tkLString     : SetStrProp(Self, GetPropertyName(lCtrl), ReadString);
			tkAString     : SetStrProp(Self, GetPropertyName(lCtrl), ReadString);
			tkWString     : SetStrProp(Self, GetPropertyName(lCtrl), ReadString);
			tkVariant     : SetVariantProp(Self, GetPropertyName(lCtrl), ReadString);
			tkWChar       : SetStrProp(Self, GetPropertyName(lCtrl), ReadString);
			tkBool        : SetOrdProp(Self, GetPropertyName(lCtrl), ReadBoolean);
			tkInt64       : SetInt64Prop(Self, GetPropertyName(lCtrl), ReadInt64);
			tkQWord       : SetInt64Prop(Self, GetPropertyName(lCtrl), ReadInt64);
		End;
End;

Procedure TRTTIObject.SaveRTTIToStream(Const aStream : TStream);

	Procedure WriteInteger(Const aInteger : Integer);
	Begin
		aStream.WriteBuffer(aInteger, SizeOf(Integer));
	End;

	Procedure WriteChar(Const aChar : String);
	Var
		lFirstChar : Char;
	Begin
		lFirstChar := aChar[1];
		aStream.WriteBuffer(lFirstChar, SizeOf(Char));
	End;

	Procedure WriteFloat(Const aFloat : Extended);
	Begin
		aStream.WriteBuffer(aFloat, SizeOf(Extended));
	End;

	Procedure WriteString(Const aString : String);
	Begin
		aStream.WriteAnsiString(aString);
	End;

	Procedure WriteBoolean(Const aBoolean : Int64);
	Var
		lRealBool : Boolean;
	Begin
		lRealBool := Boolean(aBoolean);
		aStream.WriteBuffer(lRealBool, SizeOf(Boolean));
	End;

	Procedure WriteInt64(Const aInt64 : Int64);
	Begin
		aStream.WriteBuffer(aInt64, SizeOf(Int64));
	End;

Var
	lCtrl : Integer;

Begin
	WriteString(ClassName);
	For lCtrl := 0 To PropertyCount - 1 Do
		Case GetPropertyType(lCtrl) Of
			tkInteger     : WriteInteger(GetOrdProp(Self, GetPropertyName(lCtrl)));
			tkChar        : WriteChar(GetStrProp(Self, GetPropertyName(lCtrl)));
			tkEnumeration : WriteInteger(GetOrdProp(Self, GetPropertyName(lCtrl)));
			tkFloat       : WriteFloat(GetFloatProp(Self, GetPropertyName(lCtrl)));
			tkSet         : WriteInteger(GetOrdProp(Self, GetPropertyName(lCtrl)));
			tkSString     : WriteString(GetStrProp(Self, GetPropertyName(lCtrl)));
			tkLString     : WriteString(GetStrProp(Self, GetPropertyName(lCtrl)));
			tkAString     : WriteString(GetStrProp(Self, GetPropertyName(lCtrl)));
			tkWString     : WriteString(GetStrProp(Self, GetPropertyName(lCtrl)));
			tkVariant     : WriteString(GetVariantProp(Self, GetPropertyName(lCtrl)));
			tkWChar       : WriteString(GetStrProp(Self, GetPropertyName(lCtrl)));
			tkBool        : WriteBoolean(GetOrdProp(Self, GetPropertyName(lCtrl)));
			tkInt64       : WriteInt64(GetInt64Prop(Self, GetPropertyName(lCtrl)));
			tkQWord       : WriteInt64(GetInt64Prop(Self, GetPropertyName(lCtrl)));
		End;
End;

Procedure TRTTIObject.InitRTTIFromString(Const aString : String);
Var
	lStringStream : TStringStream;
	lDummy : String;
Begin
	Try
		lStringStream := TStringStream.Create(aString);
		lDummy := lStringStream.ReadAnsiString; // Jump class name string
		InitRTTIFromStream(lStringStream);
	Finally
		lStringStream.Free;
	End;
End;

Function TRTTIObject.SaveRTTIToString: String;
Var
	lStringStream : TStringStream;
Begin
	Try
		lStringStream := TStringStream.Create('');
		SaveRTTIToStream(lStringStream);
		Result := lStringStream.DataString;
	Finally
		lStringStream.Free;
	End;
End;

Function TRTTIObject.FormattedProperties: String;
Var
	lCtrl : Integer;
Begin
	// Debug  WriteLn('TRTTIObject.FormattedProperties');
	Result := '';
	For lCtrl := 0 To PropertyCount - 1 Do
	Begin
		If lCtrl > 0 Then
			Result := Result + ' ';
		// Debug  WriteLn('Name: ', GetPropertyName(lCtrl));
		// Debug  WriteLn('Value: ', VarToStr(PropertiesByIndex[lCtrl]));
		Result := Result + GetPropertyName(lCtrl) + '="' + VarToStr(PropertiesByIndex[lCtrl]) + '"';
	End;
	// Debug  WriteLn('TRTTIObject.FormattedProperties Done.');
End;

Procedure TRTTIObject.DispatchEvent(Const aEventName : String; Const aObject : TObject);
Var
	lEvent : TRTTIEvent;
Begin
	lEvent.MsgStr := aEventName;
	lEvent.Data := Pointer(aObject);
	// Debug  WriteLn('Assigned : ', Assigned(Self));
	DispatchStr(lEvent);
End;

Function GetClassFromStream(Const aStream : TStream): String;
Begin
	Result := aStream.ReadAnsiString;
End;

Function GetClassFromString(Const aString : String): String;
Var
	lStringStream : TStringStream;
Begin
	Try
		lStringStream := TStringStream.Create(aString);
		Result := lStringStream.ReadAnsiString;	
	Finally
		lStringStream.Free;
	End;
End;

End.
