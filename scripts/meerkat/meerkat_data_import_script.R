#Script to read in meerkat GPS data from EAS data server

library(devtools)
install_github('livingingroups/cocomo', force = T)
library(cocomo)

output_dir <- '~/EAS_ind/astrandburg/data/meerkat/'

dirs_all <- c('~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2017/HM_2017_1/',
              '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2017/HM_2017_2/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2017/HM_2017_3/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2019/HM_2019_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2019/HM_2019_2/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2019/L_2019_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2021/NQ_2021_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2021/RW_2021_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2021/ZU_2021_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2021/ZU_2021_2/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2022/NQ_2022_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2022/RW_2022_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2022/SI_2022_1/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/BS_2023/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/BS_2023_2/',
               '~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/SI_2023/first/')

metadata_files <- c('~/EAS_shared/meerkat/working/METADATA/HM2017_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/HM2017_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/HM2017_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/HM2019_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/HM2019_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/L2019_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/NQ2021_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/RW2021_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/ZU2021_MOV_SUMMARY.txt',
                    '~/EAS_shared/meerkat/working/METADATA/ZU2021_MOV_SUMMARY.txt',
                    NA, NA, NA, NA, NA, NA)

output_files <- c(paste0(output_dir,'HM_2017_1_xy_level0.RData'),
                  paste0(output_dir,'HM_2017_2_xy_level0.RData'),
                  paste0(output_dir,'HM_2017_3_xy_level0.RData'),
                  paste0(output_dir,'HM_2019_1_xy_level0.RData'),
                  paste0(output_dir,'HM_2019_2_xy_level0.RData'),
                  paste0(output_dir,'L_2019_1_xy_level0.RData'),
                  paste0(output_dir,'NQ_2021_1_xy_level0.RData'),
                  paste0(output_dir,'RW_2021_1_xy_level0.RData'),
                  paste0(output_dir,'ZU_2021_1_xy_level0.RData'),
                  paste0(output_dir,'ZU_2021_2_xy_level0.RData'),
                  paste0(output_dir,'NQ_2022_1_xy_level0.RData'),
                  paste0(output_dir,'RW_2022_1_xy_level0.RData'),
                  paste0(output_dir,'SI_2022_1_xy_level0.RData'),
                  paste0(output_dir,'BS_2023_xy_level0.RData'),
                  paste0(output_dir,'BS_2023_2_xy_level0.RData'),
                  paste0(output_dir,'first_xy_level0.RData'))

tag_types <- rep('axytrek', length(dirs_all))
tag_types[1:6] <- 'gipsy5'

min_satellites <- 5

# for(i in 1:length(dirs_all)){
#   print(i)
#   input_dir <- dirs_all[i]
#   tag_type <- tag_types[i]
#
#   cocomo::import_meerkat_gps_data(input_dir = input_dir,
#                                   output_dir = output_dir,
#                                   tag_type = tag_type, min_satellites = min_satellites)
#
# }

#compare extracted data to metadata indicating which GPSs were recording for which individuals when
for(i in 1:length(dirs_all)){
  if(!is.na(metadata_files[i])){
    compare_meerkat_gps_data_to_recording_intervals(xy_file = output_files[i],
                                                            metadata_file = metadata_files[i])
  } else{
    print(paste('no metadata for file', output_files[i], '- skipping'))
  }
}


