module Tirakatar.App.Localization.Password
  (
    PasswordPageStrings(..)
  , PasswordWidgetStrings(..)
  , LoginPageStrings(..)
  , PatternPageStrings(..)
  , ConfirmEmptyPage(..)
  ) where

import Data.Time
import Data.Text (Text)

import Data.Word
import Tirakatar.App.Language

import qualified Data.Text as T

data LoginPageStrings = LPSTitle | LPSDescr

instance LocalizedPrint LoginPageStrings where
  localizedShow l v = case l of
    English -> case v of
      LPSTitle  -> "Set login name"
      LPSDescr -> "You could have several accounts and name helps to separate them."
    Russian -> case v of
      LPSTitle  -> "Установите логин"
      LPSDescr -> "Вы можете иметь несколько аккаунтов и имя поможет различать их"

data PatternPageStrings = PatPSTitle | PatPSDescr | PatPSPass | PatPSPatt | PatPSUsePass | PatPSUsePattern

instance LocalizedPrint PatternPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PatPSTitle      -> "Setup encryption pattern key for your account"
      PatPSDescr      -> "The pattern key is used every time you perform an operation with your money"
      PatPSPass       -> "Set password"
      PatPSPatt       -> "Set pattern"
      PatPSUsePass    -> "Use password"
      PatPSUsePattern -> "Use pattern"
    Russian -> case v of
      PatPSTitle      -> "Установите графический ключ шифрования для аккаунта"
      PatPSDescr      -> "Этот графический ключ используется для каждой операции с вашими деньгами"
      PatPSPass       -> "Установить пароль"
      PatPSPatt       -> "Установить ключ"
      PatPSUsePass    -> "Ввести пароль"
      PatPSUsePattern -> "Ввести ключ"

data PasswordPageStrings = PPSTitle | PPSPassTitle | PPSDescr | PPSMnemonicTitle | PPSMnemonicDescr | PPSUnlock | PPSMnemonicUnlock | PPSWrongPassword
  deriving (Eq)

instance LocalizedPrint PasswordPageStrings where
  localizedShow l v = case l of
    English -> case v of
      PPSTitle          -> "Setup login and encryption password for your account"
      PPSPassTitle      -> "Setup encryption password for your account"
      PPSDescr          -> "The password is used every time you perform an extra secure operation. Leave the password fields empty to set no password for your account (not recommended)."
      PPSMnemonicTitle  -> "Setup encryption password for your mnemonic phrase"
      PPSMnemonicDescr  -> "We ask you to set a separate password for compatibility between mobile and desktop versions of the application. Leave the fields empty to set no password for your mnemonic phrase (not recommended)."
      PPSUnlock         -> "Enter the password to unlock private storage"
      PPSMnemonicUnlock -> "Enter the password to decrypt the mnemonic phrase"
      PPSWrongPassword  -> "Wrong password"
    Russian -> case v of
      PPSTitle          -> "Установите логин и пароль для шифрования аккаунта"
      PPSPassTitle      -> "Установите пароль для шифрования аккаунта"
      PPSDescr          -> "Этот пароль используется для каждой операции, требующей секретный ключ. Можете оставить поля пароля пустыми, если хотите (не рекомендуется)."
      PPSMnemonicTitle  -> "Установите пароль для шифрования мнемонической фразы"
      PPSMnemonicDescr  -> "Мы просим Вас установить отдельный пароль для совместимости между мобильной и десктопной версией приложения. Можете оставить поля пустыми, если хотите (не рекомендуется)."
      PPSUnlock         -> "Введите пароль для расшифровки приватного хранилища"
      PPSMnemonicUnlock -> "Введите пароль для расшифровки мнемонической фразы"
      PPSWrongPassword  -> "Неверный пароль"

data PasswordWidgetStrings
  = PWSPassword
  | PWSPassNamed Text
  | PWSRepeat
  | PWSSet
  | PWSNoMatch
  | PWSGo
  | PWSLogin
  | PWSEmptyLogin
  | PWSEmptyPassword
  | PWSEmptyPattern

instance LocalizedPrint PasswordWidgetStrings where
  localizedShow l v = case l of
    English -> case v of
      PWSPassword      -> "Password"
      PWSPassNamed n   -> "Password for " <> n
      PWSRepeat        -> "Repeat password"
      PWSSet           -> "Set"
      PWSNoMatch       -> "Passwords do not match!"
      PWSGo            -> "Go"
      PWSLogin         -> "Login"
      PWSEmptyLogin    -> "Login is empty!"
      PWSEmptyPassword -> "Password is empty!"
      PWSEmptyPattern  -> ""
    Russian -> case v of
      PWSPassword      -> "Пароль"
      PWSPassNamed n   -> "Пароль от " <> n
      PWSRepeat        -> "Повторите пароль"
      PWSSet           -> "Установить"
      PWSNoMatch       -> "Пароли не совпадают!"
      PWSGo            -> "Далее"
      PWSLogin         -> "Логин"
      PWSEmptyLogin    -> "Логин пустой!"
      PWSEmptyPassword -> "Пароль пустой!"
      PWSEmptyPattern  -> ""

data ConfirmEmptyPage = CEPBack | CEPSkip | CEPAttention | CEPConsequences

instance LocalizedPrint ConfirmEmptyPage where
  localizedShow l v = case l of
    English -> case v of
      CEPBack         -> "Back"
      CEPSkip         -> "Skip"
      CEPAttention    -> "The password is empty. Are you sure?"
      CEPConsequences -> "The account will be accesible without password"
    Russian -> case v of
      CEPBack         -> "Назад"
      CEPSkip         -> "Пропустить"
      CEPAttention    -> "Пустой пароль. Вы уверены?"
      CEPConsequences -> "Аккаунт будет доступен без ввода пароля"
