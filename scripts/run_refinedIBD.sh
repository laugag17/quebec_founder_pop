#! /bin/sh

#SBATCH --account=rrg-girardsi
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=3G
#SBATCH --time=0-15:00


module load java/1.8.0_121
module load plink/2.00-10252019-avx2

## Add working dir
mydir=/your/working/path ## to change
cd ${mydir}

## Add plink bfile
plink_file = your_plink_file ## to change


for ichr in {1..22}
do
	# ichr=5
	## Make vcf
	## one duplicate...
	plink2 --bfile ${plink_file} --recode vcf --chr ${ichr}  --out ${plink_file}_chr${ichr}

	# # ## Phase with Beagle
	java -Xmx29127m -jar /the/good/path/SOFT/beagle.18May20.d20.jar gt=${plink_file}_chr${ichr}.vcf out=${plink_file}_chr${ichr}


	# ## ibd
	java -Xmx29127m -jar /the/good/path/SOFT/refined-ibd.17Jan20.102.jar gt=${plink_file}_chr${ichr}.vcf.gz out=${plink_file}_chr${ichr}

	ibd_file=${plink_file}_chr${ichr}.ibd.gz
	vcf_file=${plink_file}_chr${ichr}.vcf.gz
	map_file=/the/good/path/genetic_map_grch37_plink/plink.chr${ichr}.GRCh37.map
	gap=0.6
	discord=1
	outfile_merged=${plink_file}_chr${ichr}.merged.ibd
	# 	## Merge IBD segments..
	zcat ${ibd_file} | java -jar /the/good/path/SOFT/merge-ibd-segments.17Jan20.102.jar ${vcf_file} ${map_file} ${gap} ${discord} > ${outfile_merged}

done

