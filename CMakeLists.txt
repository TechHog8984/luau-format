cmake_minimum_required(VERSION 3.15)

project(luau-format)

set(HEADERS
    dependencies/Luau/Analysis/include/Luau/Anyification.h
    dependencies/Luau/Analysis/include/Luau/ApplyTypeFunction.h
    dependencies/Luau/Analysis/include/Luau/AstJsonEncoder.h
    dependencies/Luau/Analysis/include/Luau/AstQuery.h
    dependencies/Luau/Analysis/include/Luau/Autocomplete.h
    dependencies/Luau/Analysis/include/Luau/BuiltinDefinitions.h
    dependencies/Luau/Analysis/include/Luau/Cancellation.h
    dependencies/Luau/Analysis/include/Luau/Clone.h
    dependencies/Luau/Analysis/include/Luau/Constraint.h
    dependencies/Luau/Analysis/include/Luau/ConstraintGenerator.h
    dependencies/Luau/Analysis/include/Luau/ConstraintSolver.h
    dependencies/Luau/Analysis/include/Luau/ControlFlow.h
    dependencies/Luau/Analysis/include/Luau/DataFlowGraph.h
    dependencies/Luau/Analysis/include/Luau/DcrLogger.h
    dependencies/Luau/Analysis/include/Luau/Def.h
    dependencies/Luau/Analysis/include/Luau/Differ.h
    dependencies/Luau/Analysis/include/Luau/Documentation.h
    dependencies/Luau/Analysis/include/Luau/Error.h
    dependencies/Luau/Analysis/include/Luau/FileResolver.h
    dependencies/Luau/Analysis/include/Luau/Frontend.h
    dependencies/Luau/Analysis/include/Luau/Generalization.h
    dependencies/Luau/Analysis/include/Luau/GlobalTypes.h
    dependencies/Luau/Analysis/include/Luau/InsertionOrderedMap.h
    dependencies/Luau/Analysis/include/Luau/Instantiation.h
    dependencies/Luau/Analysis/include/Luau/Instantiation2.h
    dependencies/Luau/Analysis/include/Luau/IostreamHelpers.h
    dependencies/Luau/Analysis/include/Luau/JsonEmitter.h
    dependencies/Luau/Analysis/include/Luau/LValue.h
    dependencies/Luau/Analysis/include/Luau/Linter.h
    dependencies/Luau/Analysis/include/Luau/Metamethods.h
    dependencies/Luau/Analysis/include/Luau/Module.h
    dependencies/Luau/Analysis/include/Luau/ModuleResolver.h
    dependencies/Luau/Analysis/include/Luau/NonStrictTypeChecker.h
    dependencies/Luau/Analysis/include/Luau/Normalize.h
    dependencies/Luau/Analysis/include/Luau/NotNull.h
    dependencies/Luau/Analysis/include/Luau/OverloadResolution.h
    dependencies/Luau/Analysis/include/Luau/Predicate.h
    dependencies/Luau/Analysis/include/Luau/Quantify.h
    dependencies/Luau/Analysis/include/Luau/RecursionCounter.h
    dependencies/Luau/Analysis/include/Luau/Refinement.h
    dependencies/Luau/Analysis/include/Luau/RequireTracer.h
    dependencies/Luau/Analysis/include/Luau/Scope.h
    dependencies/Luau/Analysis/include/Luau/Set.h
    dependencies/Luau/Analysis/include/Luau/Simplify.h
    dependencies/Luau/Analysis/include/Luau/Substitution.h
    dependencies/Luau/Analysis/include/Luau/Subtyping.h
    dependencies/Luau/Analysis/include/Luau/Symbol.h
    dependencies/Luau/Analysis/include/Luau/TableLiteralInference.h
    dependencies/Luau/Analysis/include/Luau/ToDot.h
    dependencies/Luau/Analysis/include/Luau/ToString.h
    dependencies/Luau/Analysis/include/Luau/TopoSortStatements.h
    dependencies/Luau/Analysis/include/Luau/Transpiler.h
    dependencies/Luau/Analysis/include/Luau/TxnLog.h
    dependencies/Luau/Analysis/include/Luau/Type.h
    dependencies/Luau/Analysis/include/Luau/TypeArena.h
    dependencies/Luau/Analysis/include/Luau/TypeAttach.h
    dependencies/Luau/Analysis/include/Luau/TypeCheckLimits.h
    dependencies/Luau/Analysis/include/Luau/TypeChecker2.h
    dependencies/Luau/Analysis/include/Luau/TypeFamily.h
    dependencies/Luau/Analysis/include/Luau/TypeFamilyReductionGuesser.h
    dependencies/Luau/Analysis/include/Luau/TypeFwd.h
    dependencies/Luau/Analysis/include/Luau/TypeInfer.h
    dependencies/Luau/Analysis/include/Luau/TypeOrPack.h
    dependencies/Luau/Analysis/include/Luau/TypePack.h
    dependencies/Luau/Analysis/include/Luau/TypePairHash.h
    dependencies/Luau/Analysis/include/Luau/TypePath.h
    dependencies/Luau/Analysis/include/Luau/TypeUtils.h
    dependencies/Luau/Analysis/include/Luau/TypedAllocator.h
    dependencies/Luau/Analysis/include/Luau/Unifiable.h
    dependencies/Luau/Analysis/include/Luau/Unifier.h
    dependencies/Luau/Analysis/include/Luau/Unifier2.h
    dependencies/Luau/Analysis/include/Luau/UnifierSharedState.h
    dependencies/Luau/Analysis/include/Luau/Variant.h
    dependencies/Luau/Analysis/include/Luau/VisitType.h

    dependencies/Luau/Ast/include/Luau/Ast.h
    dependencies/Luau/Ast/include/Luau/Confusables.h
    dependencies/Luau/Ast/include/Luau/Lexer.h
    dependencies/Luau/Ast/include/Luau/Location.h
    dependencies/Luau/Ast/include/Luau/ParseOptions.h
    dependencies/Luau/Ast/include/Luau/ParseResult.h
    dependencies/Luau/Ast/include/Luau/Parser.h
    dependencies/Luau/Ast/include/Luau/StringUtils.h
    dependencies/Luau/Ast/include/Luau/TimeTrace.h

    dependencies/Luau/Common/include/Luau/Bytecode.h
    dependencies/Luau/Common/include/Luau/BytecodeUtils.h
    dependencies/Luau/Common/include/Luau/Common.h
    dependencies/Luau/Common/include/Luau/DenseHash.h
    dependencies/Luau/Common/include/Luau/ExperimentalFlags.h
    dependencies/Luau/Common/include/Luau/VecDeque.h

    dependencies/Luau/Config/include/Luau/Config.h
    dependencies/Luau/Config/include/Luau/LinterConfig.h

    dependencies/Luau/VM/src/lapi.h
    dependencies/Luau/VM/src/lmem.h
    dependencies/Luau/VM/src/lbuffer.h
    dependencies/Luau/VM/src/lnumutils.h
    dependencies/Luau/VM/src/lbuiltins.h
    dependencies/Luau/VM/src/lobject.h
    dependencies/Luau/VM/src/lbytecode.h
    dependencies/Luau/VM/src/lstate.h
    dependencies/Luau/VM/src/lcommon.h
    dependencies/Luau/VM/src/lstring.h
    dependencies/Luau/VM/src/ldebug.h
    dependencies/Luau/VM/src/ltable.h
    dependencies/Luau/VM/src/ldo.h
    dependencies/Luau/VM/src/ltm.h
    dependencies/Luau/VM/src/lfunc.h
    dependencies/Luau/VM/src/ludata.h
    dependencies/Luau/VM/src/lgc.h
    dependencies/Luau/VM/src/lvm.h

    dependencies/Luau/VM/include/lua.h
    dependencies/Luau/VM/include/lualib.h
    dependencies/Luau/VM/include/luaconf.h

    include/cli.hpp
    include/simplifier.hpp
    include/formatter.hpp
)

set(SRC
    dependencies/Luau/Analysis/src/Anyification.cpp
    dependencies/Luau/Analysis/src/ApplyTypeFunction.cpp
    dependencies/Luau/Analysis/src/AstJsonEncoder.cpp
    dependencies/Luau/Analysis/src/AstQuery.cpp
    dependencies/Luau/Analysis/src/Autocomplete.cpp
    dependencies/Luau/Analysis/src/BuiltinDefinitions.cpp
    dependencies/Luau/Analysis/src/Clone.cpp
    dependencies/Luau/Analysis/src/Constraint.cpp
    dependencies/Luau/Analysis/src/ConstraintGenerator.cpp
    dependencies/Luau/Analysis/src/ConstraintSolver.cpp
    dependencies/Luau/Analysis/src/DataFlowGraph.cpp
    dependencies/Luau/Analysis/src/DcrLogger.cpp
    dependencies/Luau/Analysis/src/Def.cpp
    dependencies/Luau/Analysis/src/Differ.cpp
    dependencies/Luau/Analysis/src/EmbeddedBuiltinDefinitions.cpp
    dependencies/Luau/Analysis/src/Error.cpp
    dependencies/Luau/Analysis/src/Frontend.cpp
    dependencies/Luau/Analysis/src/Generalization.cpp
    dependencies/Luau/Analysis/src/GlobalTypes.cpp
    dependencies/Luau/Analysis/src/Instantiation.cpp
    dependencies/Luau/Analysis/src/Instantiation2.cpp
    dependencies/Luau/Analysis/src/IostreamHelpers.cpp
    dependencies/Luau/Analysis/src/JsonEmitter.cpp
    dependencies/Luau/Analysis/src/LValue.cpp
    dependencies/Luau/Analysis/src/Linter.cpp
    dependencies/Luau/Analysis/src/Module.cpp
    dependencies/Luau/Analysis/src/NonStrictTypeChecker.cpp
    dependencies/Luau/Analysis/src/Normalize.cpp
    dependencies/Luau/Analysis/src/OverloadResolution.cpp
    dependencies/Luau/Analysis/src/Quantify.cpp
    dependencies/Luau/Analysis/src/Refinement.cpp
    dependencies/Luau/Analysis/src/RequireTracer.cpp
    dependencies/Luau/Analysis/src/Scope.cpp
    dependencies/Luau/Analysis/src/Simplify.cpp
    dependencies/Luau/Analysis/src/Substitution.cpp
    dependencies/Luau/Analysis/src/Subtyping.cpp
    dependencies/Luau/Analysis/src/Symbol.cpp
    dependencies/Luau/Analysis/src/TableLiteralInference.cpp
    dependencies/Luau/Analysis/src/ToDot.cpp
    dependencies/Luau/Analysis/src/ToString.cpp
    dependencies/Luau/Analysis/src/TopoSortStatements.cpp
    dependencies/Luau/Analysis/src/Transpiler.cpp
    dependencies/Luau/Analysis/src/TxnLog.cpp
    dependencies/Luau/Analysis/src/Type.cpp
    dependencies/Luau/Analysis/src/TypeArena.cpp
    dependencies/Luau/Analysis/src/TypeAttach.cpp
    dependencies/Luau/Analysis/src/TypeChecker2.cpp
    dependencies/Luau/Analysis/src/TypeFamily.cpp
    dependencies/Luau/Analysis/src/TypeFamilyReductionGuesser.cpp
    dependencies/Luau/Analysis/src/TypeInfer.cpp
    dependencies/Luau/Analysis/src/TypeOrPack.cpp
    dependencies/Luau/Analysis/src/TypePack.cpp
    dependencies/Luau/Analysis/src/TypePath.cpp
    dependencies/Luau/Analysis/src/TypeUtils.cpp
    dependencies/Luau/Analysis/src/TypedAllocator.cpp
    dependencies/Luau/Analysis/src/Unifiable.cpp
    dependencies/Luau/Analysis/src/Unifier.cpp
    dependencies/Luau/Analysis/src/Unifier2.cpp

    dependencies/Luau/Ast/src/Ast.cpp
    dependencies/Luau/Ast/src/Parser.cpp
    dependencies/Luau/Ast/src/Confusables.cpp
    dependencies/Luau/Ast/src/StringUtils.cpp
    dependencies/Luau/Ast/src/Lexer.cpp
    dependencies/Luau/Ast/src/TimeTrace.cpp
    dependencies/Luau/Ast/src/Location.cpp

    dependencies/Luau/Config/src/Config.cpp
    dependencies/Luau/Config/src/LinterConfig.cpp

    dependencies/Luau/VM/src/lapi.cpp
    dependencies/Luau/VM/src/lmem.cpp
    dependencies/Luau/VM/src/laux.cpp
    dependencies/Luau/VM/src/lnumprint.cpp
    dependencies/Luau/VM/src/lbaselib.cpp
    dependencies/Luau/VM/src/lobject.cpp
    dependencies/Luau/VM/src/lbitlib.cpp
    dependencies/Luau/VM/src/loslib.cpp
    dependencies/Luau/VM/src/lbuffer.cpp
    dependencies/Luau/VM/src/lperf.cpp
    dependencies/Luau/VM/src/lbuflib.cpp
    dependencies/Luau/VM/src/lstate.cpp
    dependencies/Luau/VM/src/lbuiltins.cpp
    dependencies/Luau/VM/src/lstring.cpp
    dependencies/Luau/VM/src/lcorolib.cpp
    dependencies/Luau/VM/src/lstrlib.cpp
    dependencies/Luau/VM/src/ldblib.cpp
    dependencies/Luau/VM/src/ltable.cpp
    dependencies/Luau/VM/src/ldebug.cpp
    dependencies/Luau/VM/src/ltablib.cpp
    dependencies/Luau/VM/src/ldo.cpp
    dependencies/Luau/VM/src/ltm.cpp
    dependencies/Luau/VM/src/lfunc.cpp
    dependencies/Luau/VM/src/ludata.cpp
    dependencies/Luau/VM/src/lgc.cpp
    dependencies/Luau/VM/src/lutf8lib.cpp
    dependencies/Luau/VM/src/lgcdebug.cpp
    dependencies/Luau/VM/src/lvmexecute.cpp
    dependencies/Luau/VM/src/linit.cpp
    dependencies/Luau/VM/src/lvmload.cpp
    dependencies/Luau/VM/src/lmathlib.cpp
    dependencies/Luau/VM/src/lvmutils.cpp

    src/cli.cpp
    src/simplifier.cpp
    src/formatter.cpp

    main.cpp
)

add_executable(${PROJECT_NAME} ${SRC} ${HEADERS})
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 17)

if(MSVC)
    # To avoid having folder for each configuration when building with Visual
    set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES LIBRARY_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES LIBRARY_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/lib")
    set_target_properties(${PROJECT_NAME} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/lib")
    set_target_properties(${PROJECT_NAME} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/lib")
    set_target_properties(${PROJECT_NAME} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/lib")

    set_property(TARGET ${PROJECT_NAME} PROPERTY VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")
else()
    set_target_properties(${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")
    set_target_properties(${PROJECT_NAME} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/lib")
endif(MSVC)

target_include_directories(${PROJECT_NAME} PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")

target_include_directories(${PROJECT_NAME} PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/dependencies/Luau/Analysis/include")
target_include_directories(${PROJECT_NAME} PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/dependencies/Luau/Ast/include")
target_include_directories(${PROJECT_NAME} PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/dependencies/Luau/Common/include")
target_include_directories(${PROJECT_NAME} PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/dependencies/Luau/Config/include")
target_include_directories(${PROJECT_NAME} PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/dependencies/Luau/VM/src")
target_include_directories(${PROJECT_NAME} PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/dependencies/Luau/VM/include")
