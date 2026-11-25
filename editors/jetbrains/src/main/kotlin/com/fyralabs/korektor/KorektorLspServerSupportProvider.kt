package com.fyralabs.korektor

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.configurations.PathEnvironmentVariableUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.LspServerSupportProvider
import com.intellij.platform.lsp.api.ProjectWideLspServerDescriptor

class KorektorLspServerSupportProvider : LspServerSupportProvider {
    override fun fileOpened(
        project: Project,
        file: VirtualFile,
        serverStarter: LspServerSupportProvider.LspServerStarter,
    ) {
        if (file.extension == "spec" && korektorPath() != null) {
            serverStarter.ensureServerStarted(KorektorLspServerDescriptor(project))
        }
    }
}

private fun korektorPath(): String? = PathEnvironmentVariableUtil.findInPath("korektor")?.absolutePath

private class KorektorLspServerDescriptor(project: Project) : ProjectWideLspServerDescriptor(project, "korektor") {
    override fun isSupportedFile(file: VirtualFile): Boolean = file.extension == "spec"

    override fun createCommandLine(): GeneralCommandLine =
        GeneralCommandLine(korektorPath() ?: "korektor", "lsp")
}
