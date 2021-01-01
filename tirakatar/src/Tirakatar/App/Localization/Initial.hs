module Tirakatar.App.Localization.Initial
  (
    InitialPageStrings(..)
  ) where

import Tirakatar.App.Language

data InitialPageStrings =
    IPSCreate
  | IPSRestore
  | IPSChooseRestorationMethod
  | IPSRestoreFromMnemonic
  | IPSRestoreFromSeed
  | IPSSettings
  | IPSSelectWallet
  | IPSOtherOptions
  | IPSPinCode
  | IPSPinCodeError

instance LocalizedPrint InitialPageStrings where
  localizedShow l v = case l of
    English -> case v of
      IPSCreate                  -> "Create new account"
      IPSRestore                 -> "Restore account"
      IPSChooseRestorationMethod -> "Choose restoration method"
      IPSRestoreFromMnemonic     -> "Mnemonic phrase"
      IPSRestoreFromSeed         -> "Seed"
      IPSSelectWallet            -> "Select account"
      IPSOtherOptions            -> "Either"
      IPSPinCode                 -> "PIN code:"
      IPSPinCodeError            -> "Invalid code"
      IPSSettings                -> "Settings"
    Russian -> case v of
      IPSCreate                  -> "Создать новый аккаунт"
      IPSRestore                 -> "Восстановить аккаунт"
      IPSChooseRestorationMethod -> "Выберите метод восстановления"
      IPSRestoreFromMnemonic     -> "Мнемоническая фраза"
      IPSRestoreFromSeed         -> "Сид"
      IPSSelectWallet            -> "Выберите аккаунт"
      IPSOtherOptions            -> "Или"
      IPSPinCode                 -> "ПИН код:"
      IPSPinCodeError            -> "Неправильный код"
      IPSSettings                -> "Настройки"
