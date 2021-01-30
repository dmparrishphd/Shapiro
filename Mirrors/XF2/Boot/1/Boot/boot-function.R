# RETURNS THE RESULT OF Boot/boot-helpers.R/(list)/source.proto.packages
function(dir.where.installed, libtree.dir, proto.pkg.config.file, EnvName="ShapirEnv") {
    source(paste0(dir.where.installed, "Boot/load.packages.R"))[[1]](dir.where.installed, libtree.dir)
    source(paste0(dir.where.installed, "Boot/source.proto.packages.R"))[[1]](dir.where.installed, proto.pkg.config.file, EnvName=EnvName)
}
