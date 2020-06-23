#!/usr/bin/env bash

# This is the basic overlay set for repositories in the CI.

# Maybe we should just use Ruby to have real objects...

# : "${foo:=bar}" sets foo to "bar" if it is unset or null

########################################################################
# MathComp
########################################################################
: "${mathcomp_CI_REF:=f25ef67ad2f58a30f1e700da89811b193755d84e}"
: "${mathcomp_CI_GITURL:=https://github.com/math-comp/math-comp}"
: "${mathcomp_CI_ARCHIVEURL:=${mathcomp_CI_GITURL}/archive}"

: "${fourcolor_CI_REF:=1f1cd5e05bc193d121e78091a5817213ddbe41af}"
: "${fourcolor_CI_GITURL:=https://github.com/math-comp/fourcolor}"
: "${fourcolor_CI_ARCHIVEURL:=${fourcolor_CI_GITURL}/archive}"

: "${oddorder_CI_REF:=6a68aaf71f22eb2d5b13043f5edad40ea9836844}"
: "${oddorder_CI_GITURL:=https://github.com/math-comp/odd-order}"
: "${oddorder_CI_ARCHIVEURL:=${oddorder_CI_GITURL}/archive}"

########################################################################
# UniMath
########################################################################
: "${unimath_CI_REF:=0323114f42dc42f2cffa94fcad2cf8c979945fdb}"
: "${unimath_CI_GITURL:=https://github.com/UniMath/UniMath}"
: "${unimath_CI_ARCHIVEURL:=${unimath_CI_GITURL}/archive}"

########################################################################
# Unicoq + Mtac2
########################################################################
: "${unicoq_CI_REF:=0c64c8d81b27d1f37abad5446ebb20442ac88184}"
: "${unicoq_CI_GITURL:=https://github.com/unicoq/unicoq}"
: "${unicoq_CI_ARCHIVEURL:=${unicoq_CI_GITURL}/archive}"

: "${mtac2_CI_REF:=1aaf5e4912505b2e6a11566c686fac3f1341399c}"
: "${mtac2_CI_GITURL:=https://github.com/Mtac2/Mtac2}"
: "${mtac2_CI_ARCHIVEURL:=${mtac2_CI_GITURL}/archive}"

########################################################################
# Mathclasses + Corn
########################################################################
: "${math_classes_CI_REF:=575901570ec8b43f2ce1d4fe671ee6c75063f2ab}"
: "${math_classes_CI_GITURL:=https://github.com/coq-community/math-classes}"
: "${math_classes_CI_ARCHIVEURL:=${math_classes_CI_GITURL}/archive}"

: "${corn_CI_REF:=5d2eb57372e827189814554b6e3e93f9e963f4fa}"
: "${corn_CI_GITURL:=https://github.com/coq-community/corn}"
: "${corn_CI_ARCHIVEURL:=${corn_CI_GITURL}/archive}"

########################################################################
# Iris
########################################################################

# NB: stdpp and Iris refs are gotten from the opam files in the Iris
# and lambdaRust repos respectively.
: "${stdpp_CI_GITURL:=https://gitlab.mpi-sws.org/iris/stdpp}"
: "${stdpp_CI_ARCHIVEURL:=${stdpp_CI_GITURL}/-/archive}"

: "${iris_CI_GITURL:=https://gitlab.mpi-sws.org/iris/iris}"
: "${iris_CI_ARCHIVEURL:=${iris_CI_GITURL}/-/archive}"

: "${lambda_rust_CI_REF:=bcf15e3dd8651eaccd4c86c25cd2533346a129ff}"
: "${lambda_rust_CI_GITURL:=https://gitlab.mpi-sws.org/iris/lambda-rust}"
: "${lambda_rust_CI_ARCHIVEURL:=${lambda_rust_CI_GITURL}/-/archive}"

########################################################################
# HoTT
########################################################################
: "${hott_CI_REF:=1f6ce1c1baedfaa7f170f32aebea7b83054a5fc4}"
: "${hott_CI_GITURL:=https://github.com/HoTT/HoTT}"
: "${hott_CI_ARCHIVEURL:=${hott_CI_GITURL}/archive}"

########################################################################
# CoqHammer
########################################################################
: "${coqhammer_CI_REF:=39184be71da29710e7486360d178f2e1a7a93c00}"
: "${coqhammer_CI_GITURL:=https://github.com/lukaszcz/coqhammer}"
: "${coqhammer_CI_ARCHIVEURL:=${coqhammer_CI_GITURL}/archive}"

########################################################################
# GeoCoq
########################################################################
: "${geocoq_CI_REF:=61d2d768c033b88552a2a9a2806b3fce3684f8c6}"
: "${geocoq_CI_GITURL:=https://github.com/GeoCoq/GeoCoq}"
: "${geocoq_CI_ARCHIVEURL:=${geocoq_CI_GITURL}/archive}"

########################################################################
# Flocq
########################################################################
: "${flocq_CI_REF:=934b3d53739e0a6a278c170a27b8e6093db44050}"
: "${flocq_CI_GITURL:=https://gitlab.inria.fr/flocq/flocq}"
: "${flocq_CI_ARCHIVEURL:=${flocq_CI_GITURL}/-/archive}"

########################################################################
# coq-performance-tests
########################################################################
: "${coq_performance_tests_CI_REF:=master}"
: "${coq_performance_tests_CI_GITURL:=https://github.com/coq-community/coq-performance-tests}"
: "${coq_performance_tests_CI_ARCHIVEURL:=${coq_performance_tests_CI_GITURL}/archive}"

########################################################################
# coq-tools
########################################################################
: "${coq_tools_CI_REF:=52db480de17aee2df2135e464f5c43b1d625d72e}"
: "${coq_tools_CI_GITURL:=https://github.com/JasonGross/coq-tools}"
: "${coq_tools_CI_ARCHIVEURL:=${coq_tools_CI_GITURL}/archive}"

########################################################################
# Coquelicot
########################################################################
: "${coquelicot_CI_REF:=4b000e5220d7b30ebc323564c10c60f6e2d22733}"
: "${coquelicot_CI_GITURL:=https://gitlab.inria.fr/coquelicot/coquelicot}"
: "${coquelicot_CI_ARCHIVEURL:=${coquelicot_CI_GITURL}/-/archive}"

########################################################################
# Coq-interval
########################################################################
: "${interval_CI_REF:=d5f1cc2ec172f55bb1d904bf48ee714bfb7c3704}"
: "${interval_CI_GITURL:=https://gitlab.inria.fr/coqinterval/interval}"
: "${interval_CI_ARCHIVEURL:=${interval_CI_GITURL}/-/archive}"

########################################################################
# Gappa stand alone tool
########################################################################
: "${gappa_tool_CI_REF:=f53e105cd73484fc76eb58ba24ead73be502c608}"
: "${gappa_tool_CI_GITURL:=https://gitlab.inria.fr/gappa/gappa}"
: "${gappa_tool_CI_ARCHIVEURL:=${gappa_tool_CI_GITURL}/-/archive}"

########################################################################
# Gappa plugin
########################################################################
: "${gappa_plugin_CI_REF:=3a367f9b7f67fb4d23a055a4c196c4b0f204c790}"
: "${gappa_plugin_CI_GITURL:=https://gitlab.inria.fr/gappa/coq}"
: "${gappa_plugin_CI_ARCHIVEURL:=${gappa_plugin_CI_GITURL}/-/archive}"

########################################################################
# CompCert
########################################################################
: "${compcert_CI_REF:=5b8578daeed73483b130618c954abb24afa8ddb9}"
: "${compcert_CI_GITURL:=https://github.com/AbsInt/CompCert}"
: "${compcert_CI_ARCHIVEURL:=${compcert_CI_GITURL}/archive}"

########################################################################
# VST
########################################################################
: "${vst_CI_REF:=96c92ea57f5285e0eb4a84d5939e9e1abf2def67}"
: "${vst_CI_GITURL:=https://github.com/PrincetonUniversity/VST}"
: "${vst_CI_ARCHIVEURL:=${vst_CI_GITURL}/archive}"

########################################################################
# cross-crypto
########################################################################
: "${cross_crypto_CI_REF:=efe3d913c155e5b45c26c824603eb47ef67616d7}"
: "${cross_crypto_CI_GITURL:=https://github.com/mit-plv/cross-crypto}"
: "${cross_crypto_CI_ARCHIVEURL:=${cross_crypto_CI_GITURL}/archive}"

########################################################################
# rewriter
########################################################################
: "${rewriter_CI_REF:=1e47fca6eb33b149119025be063c935f0393b76a}"
: "${rewriter_CI_GITURL:=https://github.com/mit-plv/rewriter}"
: "${rewriter_CI_ARCHIVEURL:=${rewriter_CI_GITURL}/archive}"

########################################################################
# fiat_parsers
########################################################################
: "${fiat_parsers_CI_REF:=4a72bdf43f0a9ce36e7a4fa20bdcb79e75658429}"
: "${fiat_parsers_CI_GITURL:=https://github.com/mit-plv/fiat}"
: "${fiat_parsers_CI_ARCHIVEURL:=${fiat_parsers_CI_GITURL}/archive}"

########################################################################
# fiat_crypto
########################################################################
: "${fiat_crypto_CI_REF:=f4e03fb070c080caeb4dda771945397db9804bc3}"
: "${fiat_crypto_CI_GITURL:=https://github.com/mit-plv/fiat-crypto}"
: "${fiat_crypto_CI_ARCHIVEURL:=${fiat_crypto_CI_GITURL}/archive}"

########################################################################
# fiat_crypto_legacy
########################################################################
: "${fiat_crypto_legacy_CI_REF:=sp2019latest}"
: "${fiat_crypto_legacy_CI_GITURL:=https://github.com/mit-plv/fiat-crypto}"
: "${fiat_crypto_legacy_CI_ARCHIVEURL:=${fiat_crypto_legacy_CI_GITURL}/archive}"

########################################################################
# coq_dpdgraph
########################################################################
: "${coq_dpdgraph_CI_REF:=acd7c15cf6ca33c00f39092716936c2d0c0e40dc}"
: "${coq_dpdgraph_CI_GITURL:=https://github.com/Karmaki/coq-dpdgraph}"
: "${coq_dpdgraph_CI_ARCHIVEURL:=${coq_dpdgraph_CI_GITURL}/archive}"

########################################################################
# CoLoR
########################################################################
: "${color_CI_REF:=1f1cd5e05bc193d121e78091a5817213ddbe41af}"
: "${color_CI_GITURL:=https://github.com/fblanqui/color}"
: "${color_CI_ARCHIVEURL:=${color_CI_GITURL}/archive}"

########################################################################
# TLC
########################################################################
: "${tlc_CI_REF:=0cf2a7f95f09ffe54854bc29dd93869dc2fd091e}"
: "${tlc_CI_GITURL:=https://github.com/charguer/tlc}"
: "${tlc_CI_ARCHIVEURL:=${tlc_CI_GITURL}/archive}"

########################################################################
# Bignums
########################################################################
: "${bignums_CI_REF:=b91c7744768a222e01d58375480ef48ea351feef}"
: "${bignums_CI_GITURL:=https://github.com/coq/bignums}"
: "${bignums_CI_ARCHIVEURL:=${bignums_CI_GITURL}/archive}"

########################################################################
# coqprime
########################################################################
: "${coqprime_CI_REF:=b934efcb6eea3bbdc88ba9350f32a772f7b3433a}"
: "${coqprime_CI_GITURL:=https://github.com/thery/coqprime}"
: "${coqprime_CI_ARCHIVEURL:=${coqprime_CI_GITURL}/archive}"

########################################################################
# bbv
########################################################################
: "${bbv_CI_REF:=9ee6027ceb931700ad19c09da7830b47c004743f}"
: "${bbv_CI_GITURL:=https://github.com/mit-plv/bbv}"
: "${bbv_CI_ARCHIVEURL:=${bbv_CI_GITURL}/archive}"

########################################################################
# bedrock2
########################################################################
: "${bedrock2_CI_REF:=720023ddce9ba114f451cf3f0ebc8f9ee3002893}"
: "${bedrock2_CI_GITURL:=https://github.com/mit-plv/bedrock2}"
: "${bedrock2_CI_ARCHIVEURL:=${bedrock2_CI_GITURL}/archive}"

########################################################################
# Equations
########################################################################
: "${equations_CI_REF:=152fb413812b38ea626c4628362c5ef46cf592bd}"
: "${equations_CI_GITURL:=https://github.com/mattam82/Coq-Equations}"
: "${equations_CI_ARCHIVEURL:=${equations_CI_GITURL}/archive}"

########################################################################
# Elpi + Hierarchy Builder
########################################################################
: "${elpi_CI_REF:=0ec84428d92703abfa92424fb84dfdd8ce0bc45a}"
: "${elpi_CI_GITURL:=https://github.com/LPCIC/coq-elpi}"
: "${elpi_CI_ARCHIVEURL:=${elpi_CI_GITURL}/archive}"

: "${elpi_hb_CI_REF:=d450025f89fccb1c48c82b7747321367868fa5e1}"
: "${elpi_hb_CI_GITURL:=https://github.com/math-comp/hierarchy-builder}"
: "${elpi_hb_CI_ARCHIVEURL:=${elpi_hb_CI_GITURL}/archive}"

########################################################################
# Engine-Bench
########################################################################
: "${engine_bench_CI_REF:=02fd5bd6a9cb73265155b65dfde55734dca6ff46}"
: "${engine_bench_CI_GITURL:=https://github.com/mit-plv/engine-bench}"
: "${engine_bench_CI_ARCHIVEURL:=${engine_bench_CI_GITURL}/archive}"

########################################################################
# fcsl-pcm
########################################################################
: "${fcsl_pcm_CI_REF:=fab4dfe3ca58ecf8aefeb8fa4ac4a2659b231f24}"
: "${fcsl_pcm_CI_GITURL:=https://github.com/imdea-software/fcsl-pcm}"
: "${fcsl_pcm_CI_ARCHIVEURL:=${fcsl_pcm_CI_GITURL}/archive}"

########################################################################
# ext-lib
########################################################################
: "${ext_lib_CI_REF:=ae627bc2417e6a4362d7c055e66a1d045f11269f}"
: "${ext_lib_CI_GITURL:=https://github.com/coq-community/coq-ext-lib}"
: "${ext_lib_CI_ARCHIVEURL:=${ext_lib_CI_GITURL}/archive}"

########################################################################
# simple-io
########################################################################
: "${simple_io_CI_REF:=3057be84fb858147edfc64bad204d53caff0be15}"
: "${simple_io_CI_GITURL:=https://github.com/Lysxia/coq-simple-io}"
: "${simple_io_CI_ARCHIVEURL:=${simple_io_CI_GITURL}/archive}"

########################################################################
# quickchick
########################################################################
: "${quickchick_CI_REF:=2d430e638124af66a343bec51243d1adc182a8cf}"
: "${quickchick_CI_GITURL:=https://github.com/QuickChick/QuickChick}"
: "${quickchick_CI_ARCHIVEURL:=${quickchick_CI_GITURL}/archive}"

########################################################################
# reduction-effects
########################################################################
: "${reduction_effects_CI_REF:=36299f58d689d8ce2dc820a993313a7bfdfb42a1}"
: "${reduction_effects_CI_GITURL:=https://github.com/coq-community/reduction-effects}"
: "${reduction_effects_CI_ARCHIVEURL:=${reduction_effects_CI_GITURL}/archive}"

########################################################################
# menhirlib
########################################################################
# Note: menhirlib is now in subfolder coq-menhirlib of menhir
: "${menhirlib_CI_REF:=043a4aaaa99d1b9e2bcca50d20d6c473a8783dee}"
: "${menhirlib_CI_GITURL:=https://gitlab.inria.fr/fpottier/menhir}"
: "${menhirlib_CI_ARCHIVEURL:=${menhirlib_CI_GITURL}/-/archive}"

########################################################################
# aac_tactics
########################################################################
: "${aac_tactics_CI_REF:=b0a1a831890d158ec38dfda229c7e10a1c5ce7d6}"
: "${aac_tactics_CI_GITURL:=https://github.com/coq-community/aac-tactics}"
: "${aac_tactics_CI_ARCHIVEURL:=${aac_tactics_CI_GITURL}/archive}"

########################################################################
# paramcoq
########################################################################
: "${paramcoq_CI_REF:=aa2f620e75a6fabe6af654a61b3959902df2d69e}"
: "${paramcoq_CI_GITURL:=https://github.com/coq-community/paramcoq}"
: "${paramcoq_CI_ARCHIVEURL:=${paramcoq_CI_GITURL}/archive}"

########################################################################
# relation_algebra
########################################################################
: "${relation_algebra_CI_REF:=c3c669003d9b3f1d0b2f97a197f81b8efd80f5b7}"
: "${relation_algebra_CI_GITURL:=https://github.com/damien-pous/relation-algebra}"
: "${relation_algebra_CI_ARCHIVEURL:=${relation_algebra_CI_GITURL}/archive}"

########################################################################
# StructTact + InfSeqExt + Cheerios + Verdi + Verdi Raft
########################################################################
: "${struct_tact_CI_REF:=f8d4f8a0e04df0522a839462e725a48d54145b48}"
: "${struct_tact_CI_GITURL:=https://github.com/uwplse/StructTact}"
: "${struct_tact_CI_ARCHIVEURL:=${struct_tact_CI_GITURL}/archive}"

: "${inf_seq_ext_CI_REF:=91b2d9bdc580c7ccb5bf2f50fffb6ebabab2715c}"
: "${inf_seq_ext_CI_GITURL:=https://github.com/DistributedComponents/InfSeqExt}"
: "${inf_seq_ext_CI_ARCHIVEURL:=${inf_seq_ext_CI_GITURL}/archive}"

: "${cheerios_CI_REF:=9c7f66e57b91f706d70afa8ed99d64ed98ab367d}"
: "${cheerios_CI_GITURL:=https://github.com/uwplse/cheerios}"
: "${cheerios_CI_ARCHIVEURL:=${cheerios_CI_GITURL}/archive}"

: "${verdi_CI_REF:=35508f2af94f9da979ece0cbdfa191019f2c5478}"
: "${verdi_CI_GITURL:=https://github.com/uwplse/verdi}"
: "${verdi_CI_ARCHIVEURL:=${verdi_CI_GITURL}/archive}"

: "${verdi_raft_CI_REF:=f3cedc031580dfd5564408b898f6c21000e82a86}"
: "${verdi_raft_CI_GITURL:=https://github.com/uwplse/verdi-raft}"
: "${verdi_raft_CI_ARCHIVEURL:=${verdi_raft_CI_GITURL}/archive}"

########################################################################
# stdlib2
########################################################################
: "${stdlib2_CI_REF:=61fdb3649e00c4b713614f165161011ae545aacf}"
: "${stdlib2_CI_GITURL:=https://github.com/coq/stdlib2}"
: "${stdlib2_CI_ARCHIVEURL:=${stdlib2_CI_GITURL}/archive}"

########################################################################
# argosy
########################################################################
: "${argosy_CI_REF:=13c3ae7e1bcb85821c1b388d47fe6d3460203b4b}"
: "${argosy_CI_GITURL:=https://github.com/mit-pdos/argosy}"
: "${argosy_CI_ARCHIVEURL:=${argosy_CI_GITURL}/archive}"

########################################################################
# perennial
########################################################################
: "${perennial_CI_REF:=coq/tested}"
: "${perennial_CI_GITURL:=https://github.com/mit-pdos/perennial}"
: "${perennial_CI_ARCHIVEURL:=${perennial_CI_GITURL}/archive}"

########################################################################
# metacoq
########################################################################
: "${metacoq_CI_REF:=9e69994145e75f229c543902e8abecb6fced3527}"
: "${metacoq_CI_GITURL:=https://github.com/MetaCoq/metacoq}"
: "${metacoq_CI_ARCHIVEURL:=${metacoq_CI_GITURL}/archive}"

########################################################################
# SF suite
########################################################################
: "${sf_CI_REF:=347596d0086d805be8c9586bd932da833eaa51d4}"
: "${sf_CI_GITURL:=https://github.com/DeepSpec/sf}"
: "${sf_CI_ARCHIVEURL:=${sf_CI_GITURL}/archive}"
