package com.fyralabs.korektor

import com.intellij.lang.Language
import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

object RpmSpecLanguage : Language("RPM Spec")

class RpmSpecFileType private constructor() : LanguageFileType(RpmSpecLanguage) {
    override fun getName(): String = "RPM Spec"

    override fun getDescription(): String = "RPM spec file"

    override fun getDefaultExtension(): String = "spec"

    override fun getIcon(): Icon? = null

    companion object {
        @JvmField
        val INSTANCE = RpmSpecFileType()
    }
}
