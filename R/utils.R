#' 生成文件校验和
#'
#' @description 为指定文件生成SHA256和MD5校验和
#'
#' @param file_path 文件路径
#'
#' @return 包含校验和信息的列表
#' @export
#'
#' @examples
#' \dontrun{
#' checksums <- generateChecksums("path/to/package.tar.gz")
#' print(checksums$sha256)
#' print(checksums$md5)
#' }
generateChecksums <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("文件不存在: ", file_path)
  }
  
  message("正在计算文件校验和: ", basename(file_path))
  
  # 计算SHA256和MD5
  sha256 <- digest::digest(file_path, algo = "sha256", file = TRUE)
  md5 <- digest::digest(file_path, algo = "md5", file = TRUE)
  
  # 获取文件大小
  file_info <- file.info(file_path)
  file_size <- file_info$size
  
  result <- list(
    file = file_path,
    sha256 = sha256,
    md5 = md5,
    size = file_size,
    modified_time = file_info$mtime
  )
  
  message("SHA256: ", sha256)
  message("MD5: ", md5)
  message("大小: ", round(file_size / 1024 / 1024, 2), " MB")
  
  return(result)
}

#' 选择版本信息
#'
#' @description 根据目标版本或当前版本从`pkg_info$versions`中选择对应的版本信息；
#' 如果未提供版本且`current_version`无法匹配，则回退到可用的最高版本。
#'
#' @param pkg_info 包信息列表，包含`current_version`与`versions`字段
#' @param version 目标版本，默认NULL表示使用`current_version`
#'
#' @return 匹配到的版本信息列表
#' @keywords internal
selectVersionInfo <- function(pkg_info, version = NULL) {
  # 基本校验
  if (is.null(pkg_info$versions) || length(pkg_info$versions) == 0) {
    stop("包无可用版本")
  }

  target_version <- if (is.null(version)) pkg_info$current_version else version

  # 优先精确匹配目标版本
  if (!is.null(target_version) && nzchar(target_version)) {
    for (v in pkg_info$versions) {
      if (!is.null(v$version) && identical(v$version, target_version)) {
        return(v)
      }
    }
  }

  # 回退：选择最高版本（稳妥，不依赖列表顺序）
  chosen <- pkg_info$versions[[1]]
  for (v in pkg_info$versions) {
    if (!is.null(v$version) && !is.null(chosen$version)) {
      if (utils::compareVersion(v$version, chosen$version) > 0) {
        chosen <- v
      }
    }
  }

  # 若是显式指定的version但未找到，抛出错误；
  # 若是默认current_version未匹配，则仅警告并回退。
  if (!is.null(version)) {
    stop("未找到版本: ", version,
         "\n可用版本: ",
         paste(sapply(pkg_info$versions, function(x) x$version), collapse = ", "))
  } else if (!is.null(target_version) && nzchar(target_version) && !identical(chosen$version, target_version)) {
    warning("未在versions中找到current_version '", target_version, "'，已回退到可用的最新版本 ", chosen$version)
  }

  return(chosen)
}

#' 生成包元数据
#'
#' @description 扫描包目录并生成完整的packages.json元数据文件
#'
#' @param packages_dir 包文件目录路径
#' @param output_file 输出的JSON文件路径，默认为"metadata/packages.json"
#' @param base_info 仓库基本信息列表，包含name、description、version
#'
#' @return 生成的元数据列表
#' @export
#'
#' @examples
#' \dontrun{
#' # 扫描packages目录生成元数据
#' metadata <- generatePackageMetadata("packages/")
#' 
#' # 自定义仓库信息
#' base_info <- list(
#'   name = "我的R包仓库",
#'   description = "私有R包仓库",
#'   version = "1.0.0"
#' )
#' metadata <- generatePackageMetadata("packages/", base_info = base_info)
#' }
generatePackageMetadata <- function(packages_dir, 
                                  output_file = "metadata/packages.json",
                                  base_info = NULL) {
  
  if (!dir.exists(packages_dir)) {
    stop("包目录不存在: ", packages_dir)
  }
  
  # 默认仓库信息
  if (is.null(base_info)) {
    base_info <- list(
      name = "zstats私有包仓库",
      description = "zstats团队私有R包仓库",
      version = "1.0.0"
    )
  }
  
  message("扫描包目录: ", packages_dir)
  
  # 获取所有包子目录
  package_dirs <- list.dirs(packages_dir, full.names = TRUE, recursive = FALSE)
  package_dirs <- package_dirs[basename(package_dirs) != ""]
  
  if (length(package_dirs) == 0) {
    stop("未找到任何包目录")
  }
  
  packages_metadata <- list()
  
  # 遍历每个包目录
  for (pkg_dir in package_dirs) {
    pkg_name <- basename(pkg_dir)
    message("\n处理包: ", pkg_name)
    
    # 获取包的所有版本文件（支持 .tar.gz 和 .tgz 格式）
    tar_files <- c(
      list.files(pkg_dir, pattern = "\\.tar\\.gz$", full.names = TRUE),
      list.files(pkg_dir, pattern = "\\.tgz$", full.names = TRUE)
    )
    
    if (length(tar_files) == 0) {
      warning("包 '", pkg_name, "' 目录下没有找到.tar.gz或.tgz文件")
      next
    }
    
    # 解析版本信息
    versions_info <- list()
    for (tar_file in tar_files) {
      # 从文件名提取版本号
      filename <- basename(tar_file)
      version <- NULL
      
      # 支持 .tar.gz 格式
      if (grepl("\\.tar\\.gz$", filename)) {
        version_match <- regmatches(filename, regexpr("_([0-9\\.\\-]+)\\.tar\\.gz$", filename))
        if (length(version_match) > 0) {
          version <- sub("^_", "", sub("\\.tar\\.gz$", "", version_match))
        }
      } else if (grepl("\\.tgz$", filename)) {
        # 支持 .tgz 格式（Mac 打包常用）
        version_match <- regmatches(filename, regexpr("_([0-9\\.\\-]+)\\.tgz$", filename))
        if (length(version_match) > 0) {
          version <- sub("^_", "", sub("\\.tgz$", "", version_match))
        }
      }
      
      if (is.null(version) || length(version) == 0) {
        warning("无法从文件名解析版本: ", filename)
        next
      }
      
      # 生成校验和
      checksums <- generateChecksums(tar_file)
      
      # 构建相对路径
      relative_path <- file.path("packages", pkg_name, filename)
      
      # 获取修改时间作为发布日期
      release_date <- as.character(as.Date(checksums$modified_time))
      
      version_info <- list(
        version = version,
        release_date = release_date,
        file = relative_path,
        sha256 = checksums$sha256,
        md5 = checksums$md5,
        size = checksums$size,
        changes = "版本更新", # 默认值，可后续手动编辑
        breaking_changes = FALSE
      )
      
      versions_info[[length(versions_info) + 1]] <- version_info
    }
    
    if (length(versions_info) == 0) {
      next
    }
    
    # 按版本排序（最新的在前）
    version_numbers <- sapply(versions_info, function(v) v$version)
    sorted_indices <- order(sapply(version_numbers, function(v) {
      parts <- as.numeric(strsplit(v, "\\.")[[1]])
      # 简单的版本比较，可以改进
      sum(parts * c(1000000, 1000, 1))
    }), decreasing = TRUE)
    
    versions_info <- versions_info[sorted_indices]
    current_version <- versions_info[[1]]$version
    
    # 构建包元数据
    pkg_metadata <- list(
      name = pkg_name,
      current_version = current_version,
      description = paste("R包", pkg_name, "的描述"), # 默认描述
      description_cn = paste("R包", pkg_name, "的中文描述"),
      author = "包作者", # 默认作者
      dependencies = c("R (>= 4.0.0)"), # 默认依赖
      versions = versions_info,
      tags = c("r-package"), # 默认标签
      license = "MIT",
      homepage = paste0("https://github.com/zstats/", pkg_name)
    )
    
    packages_metadata[[length(packages_metadata) + 1]] <- pkg_metadata
  }
  
  # 构建完整的元数据
  full_metadata <- list(
    last_updated = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    repository_info = base_info,
    packages = packages_metadata
  )
  
  # 创建输出目录
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 写入JSON文件
  json_content <- jsonlite::toJSON(full_metadata, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, output_file, useBytes = TRUE)
  
  message("\n元数据文件已生成: ", output_file)
  message("包含 ", length(packages_metadata), " 个包")
  
  return(full_metadata)
}

#' 比较依赖变更
#'
#' @description 比较两个版本之间的依赖变更
#'
#' @param old_deps 旧版本的依赖信息
#' @param new_deps 新版本的依赖信息
#'
#' @return 依赖变更信息列表
#' @keywords internal
compareDependencyChanges <- function(old_deps, new_deps) {
  changes <- list(
    added = list(),
    removed = list(),
    updated = list()
  )
  
  # 如果old_deps为NULL或没有依赖信息，所有新依赖都是added
  if (is.null(old_deps) || (is.null(old_deps$cran_packages) && is.null(old_deps$oss_packages))) {
    # 添加所有CRAN包依赖
    if (!is.null(new_deps$cran_packages)) {
      for (dep in new_deps$cran_packages) {
        changes$added <- append(changes$added, list(list(
          type = "cran",
          name = dep$name,
          version = dep$version
        )))
      }
    }
    
    # 添加所有OSS包依赖
    if (!is.null(new_deps$oss_packages)) {
      for (dep in new_deps$oss_packages) {
        changes$added <- append(changes$added, list(list(
          type = "oss",
          name = dep$name,
          version = dep$version
        )))
      }
    }
    
    return(changes)
  }
  
  # 比较CRAN包依赖
  old_cran <- if (is.null(old_deps$cran_packages)) list() else old_deps$cran_packages
  new_cran <- if (is.null(new_deps$cran_packages)) list() else new_deps$cran_packages
  
  old_cran_names <- sapply(old_cran, function(x) x$name)
  new_cran_names <- sapply(new_cran, function(x) x$name)
  
  # 新增的CRAN包
  added_cran <- setdiff(new_cran_names, old_cran_names)
  for (name in added_cran) {
    dep <- new_cran[[which(new_cran_names == name)]]
    changes$added <- append(changes$added, list(list(
      type = "cran",
      name = name,
      version = dep$version
    )))
  }
  
  # 移除的CRAN包
  removed_cran <- setdiff(old_cran_names, new_cran_names)
  for (name in removed_cran) {
    dep <- old_cran[[which(old_cran_names == name)]]
    changes$removed <- append(changes$removed, list(list(
      type = "cran",
      name = name,
      version = dep$version
    )))
  }
  
  # 更新的CRAN包（版本要求变更）
  common_cran <- intersect(old_cran_names, new_cran_names)
  for (name in common_cran) {
    old_dep <- old_cran[[which(old_cran_names == name)]]
    new_dep <- new_cran[[which(new_cran_names == name)]]
    if (old_dep$version != new_dep$version) {
      changes$updated <- append(changes$updated, list(list(
        type = "cran",
        name = name,
        old_version = old_dep$version,
        new_version = new_dep$version
      )))
    }
  }
  
  # 比较OSS包依赖（类似逻辑）
  old_oss <- if (is.null(old_deps$oss_packages)) list() else old_deps$oss_packages
  new_oss <- if (is.null(new_deps$oss_packages)) list() else new_deps$oss_packages
  
  old_oss_names <- sapply(old_oss, function(x) x$name)
  new_oss_names <- sapply(new_oss, function(x) x$name)
  
  # 新增的OSS包
  added_oss <- setdiff(new_oss_names, old_oss_names)
  for (name in added_oss) {
    dep <- new_oss[[which(new_oss_names == name)]]
    changes$added <- append(changes$added, list(list(
      type = "oss",
      name = name,
      version = dep$version
    )))
  }
  
  # 移除的OSS包
  removed_oss <- setdiff(old_oss_names, new_oss_names)
  for (name in removed_oss) {
    dep <- old_oss[[which(old_oss_names == name)]]
    changes$removed <- append(changes$removed, list(list(
      type = "oss",
      name = name,
      version = dep$version
    )))
  }
  
  # 更新的OSS包
  common_oss <- intersect(old_oss_names, new_oss_names)
  for (name in common_oss) {
    old_dep <- old_oss[[which(old_oss_names == name)]]
    new_dep <- new_oss[[which(new_oss_names == name)]]
    if (old_dep$version != new_dep$version) {
      changes$updated <- append(changes$updated, list(list(
        type = "oss",
        name = name,
        old_version = old_dep$version,
        new_version = new_dep$version
      )))
    }
  }
  
  return(changes)
}

#' 创建默认依赖结构
#'
#' @description 为包创建默认的依赖结构
#'
#' @param package_name 包名
#' @param package_info 可选的包信息
#'
#' @return 默认依赖结构
#' @keywords internal
createDefaultDependencies <- function(package_name, package_info = NULL) {
  deps <- list(
    r_version = "R (>= 4.0.0)",
    oss_packages = list(),
    cran_packages = list(),
    system_dependencies = list()
  )
  
  # 如果提供了包信息，可以添加一些默认的依赖
  if (!is.null(package_info$dependencies)) {
    if (is.character(package_info$dependencies)) {
      # 旧格式的依赖，尝试解析
      for (dep_str in package_info$dependencies) {
        if (grepl("^R", dep_str)) {
          deps$r_version <- dep_str
        } else {
          # 假设是CRAN包
          deps$cran_packages <- append(deps$cran_packages, list(list(
            name = dep_str,
            version = ">= 0.0.0",
            required = TRUE,
            description = paste("依赖包", dep_str)
          )))
        }
      }
    } else if (is.list(package_info$dependencies)) {
      # 已经是新格式
      deps <- package_info$dependencies
    }
  }
  
  return(deps)
}

#' 更新单个包的元数据
#'
#' @description 在现有的packages.json中更新或添加单个包的信息
#'
#' @param package_name 包名
#' @param package_file 包文件路径(.tar.gz)
#' @param metadata_file packages.json文件路径
#' @param package_info 包的详细信息列表（可选）
#'
#' @return 更新后的元数据
#' @export
#'
#' @examples
#' \dontrun{
#' # 添加新包版本
#' updatePackageMetadata("mypackage", "packages/mypackage/mypackage_1.2.0.tar.gz")
#' 
#' # 带详细信息
#' info <- list(
#'   description = "我的包描述",
#'   author = "作者名",
#'   license = "GPL-3"
#' )
#' updatePackageMetadata("mypackage", "packages/mypackage/mypackage_1.2.0.tar.gz", 
#'                      package_info = info)
#' }
updatePackageMetadata <- function(package_name, 
                                package_file,
                                metadata_file = "metadata/packages.json",
                                package_info = NULL) {
  
  if (!file.exists(package_file)) {
    stop("包文件不存在: ", package_file)
  }
  
  # 读取现有元数据
  if (file.exists(metadata_file)) {
    metadata <- jsonlite::fromJSON(metadata_file)
  } else {
    # 创建新的元数据结构
    metadata <- list(
      last_updated = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      repository_info = list(
        name = "zstats私有包仓库",
        description = "zstats团队私有R包仓库",
        version = "1.0.0"
      ),
      packages = list()
    )
  }
  
  # 生成新版本的校验和
  checksums <- generateChecksums(package_file)
  
  # 从文件名提取版本
  filename <- basename(package_file)
  
  # 支持多种文件格式的版本提取
  version <- NULL
  if (grepl("\\.tar\\.gz$", filename)) {
    version_match <- regmatches(filename, regexpr("_([0-9\\.\\-]+)\\.tar\\.gz$", filename))
    if (length(version_match) > 0) {
      version <- sub("^_", "", sub("\\.tar\\.gz$", "", version_match))
    }
  } else if (grepl("\\.tgz$", filename)) {
    # 支持 .tgz 格式（Mac 打包常用）
    version_match <- regmatches(filename, regexpr("_([0-9\\.\\-]+)\\.tgz$", filename))
    if (length(version_match) > 0) {
      version <- sub("^_", "", sub("\\.tgz$", "", version_match))
    }
  } else if (grepl("\\.zip$", filename)) {
    version_match <- regmatches(filename, regexpr("_([0-9\\.\\-]+)\\.zip$", filename))
    if (length(version_match) > 0) {
      version <- sub("^_", "", sub("\\.zip$", "", version_match))
    }
  }
  
  if (is.null(version) || length(version) == 0) {
    stop("无法从文件名提取版本信息: ", filename)
  }
  
  # 构建相对路径
  relative_path <- file.path("packages", package_name, filename)
  
  # 查找现有包 - 安全处理空列表的情况
  pkg_index <- integer(0)
  if (length(metadata$packages) > 0) {
    # 确保所有包都有name字段，并且返回逻辑向量
    name_matches <- sapply(metadata$packages, function(p) {
      if (is.list(p) && !is.null(p$name)) {
        return(p$name == package_name)
      } else {
        return(FALSE)
      }
    })
    pkg_index <- which(name_matches)
  }
  
  # 构建新版本的依赖信息
  new_dependencies <- createDefaultDependencies(package_name, package_info)
  
  # 如果存在现有包，比较依赖变更
  dependency_changes <- list(added = list(), removed = list(), updated = list())
  if (length(pkg_index) > 0) {
    old_pkg <- metadata$packages[[pkg_index]]
    old_dependencies <- old_pkg$dependencies
    dependency_changes <- compareDependencyChanges(old_dependencies, new_dependencies)
  } else {
    # 新包，所有依赖都是新增的
    dependency_changes <- compareDependencyChanges(NULL, new_dependencies)
  }
  
  # 新版本信息
  new_version <- list(
    version = version,
    release_date = as.character(as.Date(checksums$modified_time)),
    file = relative_path,
    sha256 = checksums$sha256,
    md5 = checksums$md5,
    size = checksums$size,
    changes = "版本更新",
    breaking_changes = FALSE,
    dependency_changes = dependency_changes
  )
  
  if (length(pkg_index) > 0) {
    # 更新现有包
    pkg <- metadata$packages[[pkg_index]]
    
    # 添加新版本到版本列表开头
    pkg$versions <- append(list(new_version), pkg$versions, after = 0)
    pkg$current_version <- version
    
    # 更新依赖信息
    pkg$dependencies <- new_dependencies
    
    # 更新包信息
    if (!is.null(package_info)) {
      for (key in names(package_info)) {
        if (key != "dependencies") {  # 依赖信息已经单独处理
          pkg[[key]] <- package_info[[key]]
        }
      }
    }
    
    metadata$packages[[pkg_index]] <- pkg
  } else {
    # 添加新包
    new_package <- list(
      name = package_name,
      current_version = version,
      description = ifelse(is.null(package_info$description), 
                          paste("R包", package_name, "的描述"), 
                          package_info$description),
      description_cn = ifelse(is.null(package_info$description_cn),
                            paste("R包", package_name, "的中文描述"),
                            package_info$description_cn),
      author = ifelse(is.null(package_info$author), "包作者", package_info$author),
      maintainer = ifelse(is.null(package_info$maintainer), 
                         paste0(package_name, "@example.com"), 
                         package_info$maintainer),
      dependencies = new_dependencies,
      versions = list(new_version),
      tags = ifelse(is.null(package_info$tags), 
                    list(c("r-package")), 
                    list(package_info$tags)),
      license = ifelse(is.null(package_info$license), "MIT", package_info$license),
      homepage = ifelse(is.null(package_info$homepage),
                       paste0("https://github.com/zstats/", package_name),
                       package_info$homepage),
      repository = ifelse(is.null(package_info$repository),
                         paste0("https://github.com/zstats/", package_name, ".git"),
                         package_info$repository),
      install_priority = ifelse(is.null(package_info$install_priority), 1, package_info$install_priority)
    )
    
    metadata$packages <- append(metadata$packages, list(new_package))
  }
  
  # 更新时间戳
  metadata$last_updated <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # 写入文件
  output_dir <- dirname(metadata_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  json_content <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, metadata_file, useBytes = TRUE)
  
  message("已更新包 '", package_name, "' 版本 ", version, " 的元数据")
  
  return(metadata)
} 