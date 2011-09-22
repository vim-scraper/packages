" hints_opengl.vim
"   Author: David Odin
"   Date:   Aug 3rd, 2011
"   Version: 1
" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("b:loaded_hints_opengl")
 finish
endif
let b:loaded_hints_opengl= "v1"
let s:keepcpo          = &cpo
set cpo&vim

" ---------------------------------------------------------------------
" Hints! {{{1
inorea glVertexAttribPointer glVertexAttribPointer<c-o>:echoh HintHL<Bar>echo "glVertexAttribPointer(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glAccum glAccum<c-o>:echoh HintHL<Bar>echo "void glAccum(GLenum op, GLfloat value)"<Bar>echoh None<cr>
inorea glAlphaFunc glAlphaFunc<c-o>:echoh HintHL<Bar>echo "void glAlphaFunc(GLenum func, GLclampf ref)"<Bar>echoh None<cr>
inorea glAreTexturesResident glAreTexturesResident<c-o>:echoh HintHL<Bar>echo "GLboolean glAreTexturesResident(GLsizei n, const GLuint *textures, GLboolean *residences)"<Bar>echoh None<cr>
inorea glArrayElement glArrayElement<c-o>:echoh HintHL<Bar>echo "void glArrayElement(GLint i)"<Bar>echoh None<cr>
inorea glBegin glBegin<c-o>:echoh HintHL<Bar>echo "void glBegin(GLenum mode)"<Bar>echoh None<cr>
inorea glBindTexture glBindTexture<c-o>:echoh HintHL<Bar>echo "void glBindTexture(GLenum target, GLuint texture)"<Bar>echoh None<cr>
inorea glBitmap glBitmap<c-o>:echoh HintHL<Bar>echo "void glBitmap(GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap)"<Bar>echoh None<cr>
inorea glBlendFunc glBlendFunc<c-o>:echoh HintHL<Bar>echo "void glBlendFunc(GLenum sfactor, GLenum dfactor)"<Bar>echoh None<cr>
inorea glCallList glCallList<c-o>:echoh HintHL<Bar>echo "void glCallList(GLuint list)"<Bar>echoh None<cr>
inorea glCallLists glCallLists<c-o>:echoh HintHL<Bar>echo "void glCallLists(GLsizei n, GLenum type, const GLvoid *lists)"<Bar>echoh None<cr>
inorea glClear glClear<c-o>:echoh HintHL<Bar>echo "void glClear(GLbitfield mask)"<Bar>echoh None<cr>
inorea glClearAccum glClearAccum<c-o>:echoh HintHL<Bar>echo "void glClearAccum(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"<Bar>echoh None<cr>
inorea glClearColor glClearColor<c-o>:echoh HintHL<Bar>echo "void glClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"<Bar>echoh None<cr>
inorea glClearDepth glClearDepth<c-o>:echoh HintHL<Bar>echo "void glClearDepth(GLclampd depth)"<Bar>echoh None<cr>
inorea glClearIndex glClearIndex<c-o>:echoh HintHL<Bar>echo "void glClearIndex(GLfloat c)"<Bar>echoh None<cr>
inorea glClearStencil glClearStencil<c-o>:echoh HintHL<Bar>echo "void glClearStencil(GLint s)"<Bar>echoh None<cr>
inorea glClipPlane glClipPlane<c-o>:echoh HintHL<Bar>echo "void glClipPlane(GLenum plane, const GLdouble *equation)"<Bar>echoh None<cr>
inorea glColor3b glColor3b<c-o>:echoh HintHL<Bar>echo "void glColor3b(GLbyte red, GLbyte green, GLbyte blue)"<Bar>echoh None<cr>
inorea glColor3bv glColor3bv<c-o>:echoh HintHL<Bar>echo "void glColor3bv(const GLbyte *v)"<Bar>echoh None<cr>
inorea glColor3d glColor3d<c-o>:echoh HintHL<Bar>echo "void glColor3d(GLdouble red, GLdouble green, GLdouble blue)"<Bar>echoh None<cr>
inorea glColor3dv glColor3dv<c-o>:echoh HintHL<Bar>echo "void glColor3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glColor3f glColor3f<c-o>:echoh HintHL<Bar>echo "void glColor3f(GLfloat red, GLfloat green, GLfloat blue)"<Bar>echoh None<cr>
inorea glColor3fv glColor3fv<c-o>:echoh HintHL<Bar>echo "void glColor3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glColor3i glColor3i<c-o>:echoh HintHL<Bar>echo "void glColor3i(GLint red, GLint green, GLint blue)"<Bar>echoh None<cr>
inorea glColor3iv glColor3iv<c-o>:echoh HintHL<Bar>echo "void glColor3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glColor3s glColor3s<c-o>:echoh HintHL<Bar>echo "void glColor3s(GLshort red, GLshort green, GLshort blue)"<Bar>echoh None<cr>
inorea glColor3sv glColor3sv<c-o>:echoh HintHL<Bar>echo "void glColor3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glColor3ub glColor3ub<c-o>:echoh HintHL<Bar>echo "void glColor3ub(GLubyte red, GLubyte green, GLubyte blue)"<Bar>echoh None<cr>
inorea glColor3ubv glColor3ubv<c-o>:echoh HintHL<Bar>echo "void glColor3ubv(const GLubyte *v)"<Bar>echoh None<cr>
inorea glColor3ui glColor3ui<c-o>:echoh HintHL<Bar>echo "void glColor3ui(GLuint red, GLuint green, GLuint blue)"<Bar>echoh None<cr>
inorea glColor3uiv glColor3uiv<c-o>:echoh HintHL<Bar>echo "void glColor3uiv(const GLuint *v)"<Bar>echoh None<cr>
inorea glColor3us glColor3us<c-o>:echoh HintHL<Bar>echo "void glColor3us(GLushort red, GLushort green, GLushort blue)"<Bar>echoh None<cr>
inorea glColor3usv glColor3usv<c-o>:echoh HintHL<Bar>echo "void glColor3usv(const GLushort *v)"<Bar>echoh None<cr>
inorea glColor4b glColor4b<c-o>:echoh HintHL<Bar>echo "void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha)"<Bar>echoh None<cr>
inorea glColor4bv glColor4bv<c-o>:echoh HintHL<Bar>echo "void glColor4bv(const GLbyte *v)"<Bar>echoh None<cr>
inorea glColor4d glColor4d<c-o>:echoh HintHL<Bar>echo "void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)"<Bar>echoh None<cr>
inorea glColor4dv glColor4dv<c-o>:echoh HintHL<Bar>echo "void glColor4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glColor4f glColor4f<c-o>:echoh HintHL<Bar>echo "void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"<Bar>echoh None<cr>
inorea glColor4fv glColor4fv<c-o>:echoh HintHL<Bar>echo "void glColor4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glColor4i glColor4i<c-o>:echoh HintHL<Bar>echo "void glColor4i(GLint red, GLint green, GLint blue, GLint alpha)"<Bar>echoh None<cr>
inorea glColor4iv glColor4iv<c-o>:echoh HintHL<Bar>echo "void glColor4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glColor4s glColor4s<c-o>:echoh HintHL<Bar>echo "void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha)"<Bar>echoh None<cr>
inorea glColor4sv glColor4sv<c-o>:echoh HintHL<Bar>echo "void glColor4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glColor4ub glColor4ub<c-o>:echoh HintHL<Bar>echo "void glColor4ub(GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha)"<Bar>echoh None<cr>
inorea glColor4ubv glColor4ubv<c-o>:echoh HintHL<Bar>echo "void glColor4ubv(const GLubyte *v)"<Bar>echoh None<cr>
inorea glColor4ui glColor4ui<c-o>:echoh HintHL<Bar>echo "void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha)"<Bar>echoh None<cr>
inorea glColor4uiv glColor4uiv<c-o>:echoh HintHL<Bar>echo "void glColor4uiv(const GLuint *v)"<Bar>echoh None<cr>
inorea glColor4us glColor4us<c-o>:echoh HintHL<Bar>echo "void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha)"<Bar>echoh None<cr>
inorea glColor4usv glColor4usv<c-o>:echoh HintHL<Bar>echo "void glColor4usv(const GLushort *v)"<Bar>echoh None<cr>
inorea glColorMask glColorMask<c-o>:echoh HintHL<Bar>echo "void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)"<Bar>echoh None<cr>
inorea glColorMaterial glColorMaterial<c-o>:echoh HintHL<Bar>echo "void glColorMaterial(GLenum face, GLenum mode)"<Bar>echoh None<cr>
inorea glColorPointer glColorPointer<c-o>:echoh HintHL<Bar>echo "void glColorPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glCopyPixels glCopyPixels<c-o>:echoh HintHL<Bar>echo "void glCopyPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum type)"<Bar>echoh None<cr>
inorea glCopyTexImage1D glCopyTexImage1D<c-o>:echoh HintHL<Bar>echo "void glCopyTexImage1D(GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLint border)"<Bar>echoh None<cr>
inorea glCopyTexImage2D glCopyTexImage2D<c-o>:echoh HintHL<Bar>echo "void glCopyTexImage2D(GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)"<Bar>echoh None<cr>
inorea glCopyTexSubImage1D glCopyTexSubImage1D<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyTexSubImage2D glCopyTexSubImage2D<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glCullFace glCullFace<c-o>:echoh HintHL<Bar>echo "void glCullFace(GLenum mode)"<Bar>echoh None<cr>
inorea glDeleteLists glDeleteLists<c-o>:echoh HintHL<Bar>echo "void glDeleteLists(GLuint list, GLsizei range)"<Bar>echoh None<cr>
inorea glDeleteTextures glDeleteTextures<c-o>:echoh HintHL<Bar>echo "void glDeleteTextures(GLsizei n, const GLuint *textures)"<Bar>echoh None<cr>
inorea glDepthFunc glDepthFunc<c-o>:echoh HintHL<Bar>echo "void glDepthFunc(GLenum func)"<Bar>echoh None<cr>
inorea glDepthMask glDepthMask<c-o>:echoh HintHL<Bar>echo "void glDepthMask(GLboolean flag)"<Bar>echoh None<cr>
inorea glDepthRange glDepthRange<c-o>:echoh HintHL<Bar>echo "void glDepthRange(GLclampd zNear, GLclampd zFar)"<Bar>echoh None<cr>
inorea glDisable glDisable<c-o>:echoh HintHL<Bar>echo "void glDisable(GLenum cap)"<Bar>echoh None<cr>
inorea glDisableClientState glDisableClientState<c-o>:echoh HintHL<Bar>echo "void glDisableClientState(GLenum array)"<Bar>echoh None<cr>
inorea glDrawArrays glDrawArrays<c-o>:echoh HintHL<Bar>echo "void glDrawArrays(GLenum mode, GLint first, GLsizei count)"<Bar>echoh None<cr>
inorea glDrawBuffer glDrawBuffer<c-o>:echoh HintHL<Bar>echo "void glDrawBuffer(GLenum mode)"<Bar>echoh None<cr>
inorea glDrawElements glDrawElements<c-o>:echoh HintHL<Bar>echo "void glDrawElements(GLenum mode, GLsizei count, GLenum type, const GLvoid *indices)"<Bar>echoh None<cr>
inorea glDrawPixels glDrawPixels<c-o>:echoh HintHL<Bar>echo "void glDrawPixels(GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glEdgeFlag glEdgeFlag<c-o>:echoh HintHL<Bar>echo "void glEdgeFlag(GLboolean flag)"<Bar>echoh None<cr>
inorea glEdgeFlagPointer glEdgeFlagPointer<c-o>:echoh HintHL<Bar>echo "void glEdgeFlagPointer(GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glEdgeFlagv glEdgeFlagv<c-o>:echoh HintHL<Bar>echo "void glEdgeFlagv(const GLboolean *flag)"<Bar>echoh None<cr>
inorea glEnable glEnable<c-o>:echoh HintHL<Bar>echo "void glEnable(GLenum cap)"<Bar>echoh None<cr>
inorea glEnableClientState glEnableClientState<c-o>:echoh HintHL<Bar>echo "void glEnableClientState(GLenum array)"<Bar>echoh None<cr>
inorea glEnd glEnd<c-o>:echoh HintHL<Bar>echo "void glEnd(void)"<Bar>echoh None<cr>
inorea glEndList glEndList<c-o>:echoh HintHL<Bar>echo "void glEndList(void)"<Bar>echoh None<cr>
inorea glEvalCoord1d glEvalCoord1d<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1d(GLdouble u)"<Bar>echoh None<cr>
inorea glEvalCoord1dv glEvalCoord1dv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1dv(const GLdouble *u)"<Bar>echoh None<cr>
inorea glEvalCoord1f glEvalCoord1f<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1f(GLfloat u)"<Bar>echoh None<cr>
inorea glEvalCoord1fv glEvalCoord1fv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1fv(const GLfloat *u)"<Bar>echoh None<cr>
inorea glEvalCoord2d glEvalCoord2d<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2d(GLdouble u, GLdouble v)"<Bar>echoh None<cr>
inorea glEvalCoord2dv glEvalCoord2dv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2dv(const GLdouble *u)"<Bar>echoh None<cr>
inorea glEvalCoord2f glEvalCoord2f<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2f(GLfloat u, GLfloat v)"<Bar>echoh None<cr>
inorea glEvalCoord2fv glEvalCoord2fv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2fv(const GLfloat *u)"<Bar>echoh None<cr>
inorea glEvalMesh1 glEvalMesh1<c-o>:echoh HintHL<Bar>echo "void glEvalMesh1(GLenum mode, GLint i1, GLint i2)"<Bar>echoh None<cr>
inorea glEvalMesh2 glEvalMesh2<c-o>:echoh HintHL<Bar>echo "void glEvalMesh2(GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2)"<Bar>echoh None<cr>
inorea glEvalPoint1 glEvalPoint1<c-o>:echoh HintHL<Bar>echo "void glEvalPoint1(GLint i)"<Bar>echoh None<cr>
inorea glEvalPoint2 glEvalPoint2<c-o>:echoh HintHL<Bar>echo "void glEvalPoint2(GLint i, GLint j)"<Bar>echoh None<cr>
inorea glFeedbackBuffer glFeedbackBuffer<c-o>:echoh HintHL<Bar>echo "void glFeedbackBuffer(GLsizei size, GLenum type, GLfloat *buffer)"<Bar>echoh None<cr>
inorea glFinish glFinish<c-o>:echoh HintHL<Bar>echo "void glFinish(void)"<Bar>echoh None<cr>
inorea glFlush glFlush<c-o>:echoh HintHL<Bar>echo "void glFlush(void)"<Bar>echoh None<cr>
inorea glFogf glFogf<c-o>:echoh HintHL<Bar>echo "void glFogf(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glFogfv glFogfv<c-o>:echoh HintHL<Bar>echo "void glFogfv(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glFogi glFogi<c-o>:echoh HintHL<Bar>echo "void glFogi(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glFogiv glFogiv<c-o>:echoh HintHL<Bar>echo "void glFogiv(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glFrontFace glFrontFace<c-o>:echoh HintHL<Bar>echo "void glFrontFace(GLenum mode)"<Bar>echoh None<cr>
inorea glFrustum glFrustum<c-o>:echoh HintHL<Bar>echo "void glFrustum(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)"<Bar>echoh None<cr>
inorea glGenLists glGenLists<c-o>:echoh HintHL<Bar>echo "GLuint glGenLists(GLsizei range)"<Bar>echoh None<cr>
inorea glGenTextures glGenTextures<c-o>:echoh HintHL<Bar>echo "void glGenTextures(GLsizei n, GLuint *textures)"<Bar>echoh None<cr>
inorea glGetBooleanv glGetBooleanv<c-o>:echoh HintHL<Bar>echo "void glGetBooleanv(GLenum pname, GLboolean *params)"<Bar>echoh None<cr>
inorea glGetClipPlane glGetClipPlane<c-o>:echoh HintHL<Bar>echo "void glGetClipPlane(GLenum plane, GLdouble *equation)"<Bar>echoh None<cr>
inorea glGetDoublev glGetDoublev<c-o>:echoh HintHL<Bar>echo "void glGetDoublev(GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetError glGetError<c-o>:echoh HintHL<Bar>echo "GLenum glGetError(void)"<Bar>echoh None<cr>
inorea glGetFloatv glGetFloatv<c-o>:echoh HintHL<Bar>echo "void glGetFloatv(GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetIntegerv glGetIntegerv<c-o>:echoh HintHL<Bar>echo "void glGetIntegerv(GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetLightfv glGetLightfv<c-o>:echoh HintHL<Bar>echo "void glGetLightfv(GLenum light, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetLightiv glGetLightiv<c-o>:echoh HintHL<Bar>echo "void glGetLightiv(GLenum light, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMapdv glGetMapdv<c-o>:echoh HintHL<Bar>echo "void glGetMapdv(GLenum target, GLenum query, GLdouble *v)"<Bar>echoh None<cr>
inorea glGetMapfv glGetMapfv<c-o>:echoh HintHL<Bar>echo "void glGetMapfv(GLenum target, GLenum query, GLfloat *v)"<Bar>echoh None<cr>
inorea glGetMapiv glGetMapiv<c-o>:echoh HintHL<Bar>echo "void glGetMapiv(GLenum target, GLenum query, GLint *v)"<Bar>echoh None<cr>
inorea glGetMaterialfv glGetMaterialfv<c-o>:echoh HintHL<Bar>echo "void glGetMaterialfv(GLenum face, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMaterialiv glGetMaterialiv<c-o>:echoh HintHL<Bar>echo "void glGetMaterialiv(GLenum face, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetPixelMapfv glGetPixelMapfv<c-o>:echoh HintHL<Bar>echo "void glGetPixelMapfv(GLenum map, GLfloat *values)"<Bar>echoh None<cr>
inorea glGetPixelMapuiv glGetPixelMapuiv<c-o>:echoh HintHL<Bar>echo "void glGetPixelMapuiv(GLenum map, GLuint *values)"<Bar>echoh None<cr>
inorea glGetPixelMapusv glGetPixelMapusv<c-o>:echoh HintHL<Bar>echo "void glGetPixelMapusv(GLenum map, GLushort *values)"<Bar>echoh None<cr>
inorea glGetPointerv glGetPointerv<c-o>:echoh HintHL<Bar>echo "void glGetPointerv(GLenum pname, GLvoid* *params)"<Bar>echoh None<cr>
inorea glGetPolygonStipple glGetPolygonStipple<c-o>:echoh HintHL<Bar>echo "void glGetPolygonStipple(GLubyte *mask)"<Bar>echoh None<cr>
inorea glGetString glGetString<c-o>:echoh HintHL<Bar>echo "const GLubyte *glGetString(GLenum name)"<Bar>echoh None<cr>
inorea glGetTexEnvfv glGetTexEnvfv<c-o>:echoh HintHL<Bar>echo "void glGetTexEnvfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexEnviv glGetTexEnviv<c-o>:echoh HintHL<Bar>echo "void glGetTexEnviv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTexGendv glGetTexGendv<c-o>:echoh HintHL<Bar>echo "void glGetTexGendv(GLenum coord, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetTexGenfv glGetTexGenfv<c-o>:echoh HintHL<Bar>echo "void glGetTexGenfv(GLenum coord, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexGeniv glGetTexGeniv<c-o>:echoh HintHL<Bar>echo "void glGetTexGeniv(GLenum coord, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTexImage glGetTexImage<c-o>:echoh HintHL<Bar>echo "void glGetTexImage(GLenum target, GLint level, GLenum format, GLenum type, GLvoid *pixels)"<Bar>echoh None<cr>
inorea glGetTexLevelParameterfv glGetTexLevelParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexLevelParameteriv glGetTexLevelParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTexParameterfv glGetTexParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetTexParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexParameteriv glGetTexParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetTexParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glHint glHint<c-o>:echoh HintHL<Bar>echo "void glHint(GLenum target, GLenum mode)"<Bar>echoh None<cr>
inorea glIndexMask glIndexMask<c-o>:echoh HintHL<Bar>echo "void glIndexMask(GLuint mask)"<Bar>echoh None<cr>
inorea glIndexPointer glIndexPointer<c-o>:echoh HintHL<Bar>echo "void glIndexPointer(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glIndexd glIndexd<c-o>:echoh HintHL<Bar>echo "void glIndexd(GLdouble c)"<Bar>echoh None<cr>
inorea glIndexdv glIndexdv<c-o>:echoh HintHL<Bar>echo "void glIndexdv(const GLdouble *c)"<Bar>echoh None<cr>
inorea glIndexf glIndexf<c-o>:echoh HintHL<Bar>echo "void glIndexf(GLfloat c)"<Bar>echoh None<cr>
inorea glIndexfv glIndexfv<c-o>:echoh HintHL<Bar>echo "void glIndexfv(const GLfloat *c)"<Bar>echoh None<cr>
inorea glIndexi glIndexi<c-o>:echoh HintHL<Bar>echo "void glIndexi(GLint c)"<Bar>echoh None<cr>
inorea glIndexiv glIndexiv<c-o>:echoh HintHL<Bar>echo "void glIndexiv(const GLint *c)"<Bar>echoh None<cr>
inorea glIndexs glIndexs<c-o>:echoh HintHL<Bar>echo "void glIndexs(GLshort c)"<Bar>echoh None<cr>
inorea glIndexsv glIndexsv<c-o>:echoh HintHL<Bar>echo "void glIndexsv(const GLshort *c)"<Bar>echoh None<cr>
inorea glIndexub glIndexub<c-o>:echoh HintHL<Bar>echo "void glIndexub(GLubyte c)"<Bar>echoh None<cr>
inorea glIndexubv glIndexubv<c-o>:echoh HintHL<Bar>echo "void glIndexubv(const GLubyte *c)"<Bar>echoh None<cr>
inorea glInitNames glInitNames<c-o>:echoh HintHL<Bar>echo "void glInitNames(void)"<Bar>echoh None<cr>
inorea glInterleavedArrays glInterleavedArrays<c-o>:echoh HintHL<Bar>echo "void glInterleavedArrays(GLenum format, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glIsEnabled glIsEnabled<c-o>:echoh HintHL<Bar>echo "GLboolean glIsEnabled(GLenum cap)"<Bar>echoh None<cr>
inorea glIsList glIsList<c-o>:echoh HintHL<Bar>echo "GLboolean glIsList(GLuint list)"<Bar>echoh None<cr>
inorea glIsTexture glIsTexture<c-o>:echoh HintHL<Bar>echo "GLboolean glIsTexture(GLuint texture)"<Bar>echoh None<cr>
inorea glLightModelf glLightModelf<c-o>:echoh HintHL<Bar>echo "void glLightModelf(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glLightModelfv glLightModelfv<c-o>:echoh HintHL<Bar>echo "void glLightModelfv(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glLightModeli glLightModeli<c-o>:echoh HintHL<Bar>echo "void glLightModeli(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glLightModeliv glLightModeliv<c-o>:echoh HintHL<Bar>echo "void glLightModeliv(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glLightf glLightf<c-o>:echoh HintHL<Bar>echo "void glLightf(GLenum light, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glLightfv glLightfv<c-o>:echoh HintHL<Bar>echo "void glLightfv(GLenum light, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glLighti glLighti<c-o>:echoh HintHL<Bar>echo "void glLighti(GLenum light, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glLightiv glLightiv<c-o>:echoh HintHL<Bar>echo "void glLightiv(GLenum light, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glLineStipple glLineStipple<c-o>:echoh HintHL<Bar>echo "void glLineStipple(GLint factor, GLushort pattern)"<Bar>echoh None<cr>
inorea glLineWidth glLineWidth<c-o>:echoh HintHL<Bar>echo "void glLineWidth(GLfloat width)"<Bar>echoh None<cr>
inorea glListBase glListBase<c-o>:echoh HintHL<Bar>echo "void glListBase(GLuint base)"<Bar>echoh None<cr>
inorea glLoadIdentity glLoadIdentity<c-o>:echoh HintHL<Bar>echo "void glLoadIdentity(void)"<Bar>echoh None<cr>
inorea glLoadMatrixd glLoadMatrixd<c-o>:echoh HintHL<Bar>echo "void glLoadMatrixd(const GLdouble *m)"<Bar>echoh None<cr>
inorea glLoadMatrixf glLoadMatrixf<c-o>:echoh HintHL<Bar>echo "void glLoadMatrixf(const GLfloat *m)"<Bar>echoh None<cr>
inorea glLoadName glLoadName<c-o>:echoh HintHL<Bar>echo "void glLoadName(GLuint name)"<Bar>echoh None<cr>
inorea glLogicOp glLogicOp<c-o>:echoh HintHL<Bar>echo "void glLogicOp(GLenum opcode)"<Bar>echoh None<cr>
inorea glMap1d glMap1d<c-o>:echoh HintHL<Bar>echo "void glMap1d(GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points)"<Bar>echoh None<cr>
inorea glMap1f glMap1f<c-o>:echoh HintHL<Bar>echo "void glMap1f(GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points)"<Bar>echoh None<cr>
inorea glMap2d glMap2d<c-o>:echoh HintHL<Bar>echo "void glMap2d(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points)"<Bar>echoh None<cr>
inorea glMap2f glMap2f<c-o>:echoh HintHL<Bar>echo "void glMap2f(GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points)"<Bar>echoh None<cr>
inorea glMapGrid1d glMapGrid1d<c-o>:echoh HintHL<Bar>echo "void glMapGrid1d(GLint un, GLdouble u1, GLdouble u2)"<Bar>echoh None<cr>
inorea glMapGrid1f glMapGrid1f<c-o>:echoh HintHL<Bar>echo "void glMapGrid1f(GLint un, GLfloat u1, GLfloat u2)"<Bar>echoh None<cr>
inorea glMapGrid2d glMapGrid2d<c-o>:echoh HintHL<Bar>echo "void glMapGrid2d(GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2)"<Bar>echoh None<cr>
inorea glMapGrid2f glMapGrid2f<c-o>:echoh HintHL<Bar>echo "void glMapGrid2f(GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2)"<Bar>echoh None<cr>
inorea glMaterialf glMaterialf<c-o>:echoh HintHL<Bar>echo "void glMaterialf(GLenum face, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glMaterialfv glMaterialfv<c-o>:echoh HintHL<Bar>echo "void glMaterialfv(GLenum face, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glMateriali glMateriali<c-o>:echoh HintHL<Bar>echo "void glMateriali(GLenum face, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glMaterialiv glMaterialiv<c-o>:echoh HintHL<Bar>echo "void glMaterialiv(GLenum face, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glMatrixMode glMatrixMode<c-o>:echoh HintHL<Bar>echo "void glMatrixMode(GLenum mode)"<Bar>echoh None<cr>
inorea glMultMatrixd glMultMatrixd<c-o>:echoh HintHL<Bar>echo "void glMultMatrixd(const GLdouble *m)"<Bar>echoh None<cr>
inorea glMultMatrixf glMultMatrixf<c-o>:echoh HintHL<Bar>echo "void glMultMatrixf(const GLfloat *m)"<Bar>echoh None<cr>
inorea glNewList glNewList<c-o>:echoh HintHL<Bar>echo "void glNewList(GLuint list, GLenum mode)"<Bar>echoh None<cr>
inorea glNormal3b glNormal3b<c-o>:echoh HintHL<Bar>echo "void glNormal3b(GLbyte nx, GLbyte ny, GLbyte nz)"<Bar>echoh None<cr>
inorea glNormal3bv glNormal3bv<c-o>:echoh HintHL<Bar>echo "void glNormal3bv(const GLbyte *v)"<Bar>echoh None<cr>
inorea glNormal3d glNormal3d<c-o>:echoh HintHL<Bar>echo "void glNormal3d(GLdouble nx, GLdouble ny, GLdouble nz)"<Bar>echoh None<cr>
inorea glNormal3dv glNormal3dv<c-o>:echoh HintHL<Bar>echo "void glNormal3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glNormal3f glNormal3f<c-o>:echoh HintHL<Bar>echo "void glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz)"<Bar>echoh None<cr>
inorea glNormal3fv glNormal3fv<c-o>:echoh HintHL<Bar>echo "void glNormal3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glNormal3i glNormal3i<c-o>:echoh HintHL<Bar>echo "void glNormal3i(GLint nx, GLint ny, GLint nz)"<Bar>echoh None<cr>
inorea glNormal3iv glNormal3iv<c-o>:echoh HintHL<Bar>echo "void glNormal3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glNormal3s glNormal3s<c-o>:echoh HintHL<Bar>echo "void glNormal3s(GLshort nx, GLshort ny, GLshort nz)"<Bar>echoh None<cr>
inorea glNormal3sv glNormal3sv<c-o>:echoh HintHL<Bar>echo "void glNormal3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glNormalPointer glNormalPointer<c-o>:echoh HintHL<Bar>echo "void glNormalPointer(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glOrtho glOrtho<c-o>:echoh HintHL<Bar>echo "void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)"<Bar>echoh None<cr>
inorea glPassThrough glPassThrough<c-o>:echoh HintHL<Bar>echo "void glPassThrough(GLfloat token)"<Bar>echoh None<cr>
inorea glPixelMapfv glPixelMapfv<c-o>:echoh HintHL<Bar>echo "void glPixelMapfv(GLenum map, GLsizei mapsize, const GLfloat *values)"<Bar>echoh None<cr>
inorea glPixelMapuiv glPixelMapuiv<c-o>:echoh HintHL<Bar>echo "void glPixelMapuiv(GLenum map, GLsizei mapsize, const GLuint *values)"<Bar>echoh None<cr>
inorea glPixelMapusv glPixelMapusv<c-o>:echoh HintHL<Bar>echo "void glPixelMapusv(GLenum map, GLsizei mapsize, const GLushort *values)"<Bar>echoh None<cr>
inorea glPixelStoref glPixelStoref<c-o>:echoh HintHL<Bar>echo "void glPixelStoref(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPixelStorei glPixelStorei<c-o>:echoh HintHL<Bar>echo "void glPixelStorei(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPixelTransferf glPixelTransferf<c-o>:echoh HintHL<Bar>echo "void glPixelTransferf(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPixelTransferi glPixelTransferi<c-o>:echoh HintHL<Bar>echo "void glPixelTransferi(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPixelZoom glPixelZoom<c-o>:echoh HintHL<Bar>echo "void glPixelZoom(GLfloat xfactor, GLfloat yfactor)"<Bar>echoh None<cr>
inorea glPointSize glPointSize<c-o>:echoh HintHL<Bar>echo "void glPointSize(GLfloat size)"<Bar>echoh None<cr>
inorea glPolygonMode glPolygonMode<c-o>:echoh HintHL<Bar>echo "void glPolygonMode(GLenum face, GLenum mode)"<Bar>echoh None<cr>
inorea glPolygonOffset glPolygonOffset<c-o>:echoh HintHL<Bar>echo "void glPolygonOffset(GLfloat factor, GLfloat units)"<Bar>echoh None<cr>
inorea glPolygonStipple glPolygonStipple<c-o>:echoh HintHL<Bar>echo "void glPolygonStipple(const GLubyte *mask)"<Bar>echoh None<cr>
inorea glPopAttrib glPopAttrib<c-o>:echoh HintHL<Bar>echo "void glPopAttrib(void)"<Bar>echoh None<cr>
inorea glPopClientAttrib glPopClientAttrib<c-o>:echoh HintHL<Bar>echo "void glPopClientAttrib(void)"<Bar>echoh None<cr>
inorea glPopMatrix glPopMatrix<c-o>:echoh HintHL<Bar>echo "void glPopMatrix(void)"<Bar>echoh None<cr>
inorea glPopName glPopName<c-o>:echoh HintHL<Bar>echo "void glPopName(void)"<Bar>echoh None<cr>
inorea glPrioritizeTextures glPrioritizeTextures<c-o>:echoh HintHL<Bar>echo "void glPrioritizeTextures(GLsizei n, const GLuint *textures, const GLclampf *priorities)"<Bar>echoh None<cr>
inorea glPushAttrib glPushAttrib<c-o>:echoh HintHL<Bar>echo "void glPushAttrib(GLbitfield mask)"<Bar>echoh None<cr>
inorea glPushClientAttrib glPushClientAttrib<c-o>:echoh HintHL<Bar>echo "void glPushClientAttrib(GLbitfield mask)"<Bar>echoh None<cr>
inorea glPushMatrix glPushMatrix<c-o>:echoh HintHL<Bar>echo "void glPushMatrix(void)"<Bar>echoh None<cr>
inorea glPushName glPushName<c-o>:echoh HintHL<Bar>echo "void glPushName(GLuint name)"<Bar>echoh None<cr>
inorea glRasterPos2d glRasterPos2d<c-o>:echoh HintHL<Bar>echo "void glRasterPos2d(GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glRasterPos2dv glRasterPos2dv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glRasterPos2f glRasterPos2f<c-o>:echoh HintHL<Bar>echo "void glRasterPos2f(GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glRasterPos2fv glRasterPos2fv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glRasterPos2i glRasterPos2i<c-o>:echoh HintHL<Bar>echo "void glRasterPos2i(GLint x, GLint y)"<Bar>echoh None<cr>
inorea glRasterPos2iv glRasterPos2iv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2iv(const GLint *v)"<Bar>echoh None<cr>
inorea glRasterPos2s glRasterPos2s<c-o>:echoh HintHL<Bar>echo "void glRasterPos2s(GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glRasterPos2sv glRasterPos2sv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glRasterPos3d glRasterPos3d<c-o>:echoh HintHL<Bar>echo "void glRasterPos3d(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glRasterPos3dv glRasterPos3dv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glRasterPos3f glRasterPos3f<c-o>:echoh HintHL<Bar>echo "void glRasterPos3f(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glRasterPos3fv glRasterPos3fv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glRasterPos3i glRasterPos3i<c-o>:echoh HintHL<Bar>echo "void glRasterPos3i(GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glRasterPos3iv glRasterPos3iv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glRasterPos3s glRasterPos3s<c-o>:echoh HintHL<Bar>echo "void glRasterPos3s(GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glRasterPos3sv glRasterPos3sv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glRasterPos4d glRasterPos4d<c-o>:echoh HintHL<Bar>echo "void glRasterPos4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glRasterPos4dv glRasterPos4dv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glRasterPos4f glRasterPos4f<c-o>:echoh HintHL<Bar>echo "void glRasterPos4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glRasterPos4fv glRasterPos4fv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glRasterPos4i glRasterPos4i<c-o>:echoh HintHL<Bar>echo "void glRasterPos4i(GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glRasterPos4iv glRasterPos4iv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glRasterPos4s glRasterPos4s<c-o>:echoh HintHL<Bar>echo "void glRasterPos4s(GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glRasterPos4sv glRasterPos4sv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glReadBuffer glReadBuffer<c-o>:echoh HintHL<Bar>echo "void glReadBuffer(GLenum mode)"<Bar>echoh None<cr>
inorea glReadPixels glReadPixels<c-o>:echoh HintHL<Bar>echo "void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *pixels)"<Bar>echoh None<cr>
inorea glRectd glRectd<c-o>:echoh HintHL<Bar>echo "void glRectd(GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2)"<Bar>echoh None<cr>
inorea glRectdv glRectdv<c-o>:echoh HintHL<Bar>echo "void glRectdv(const GLdouble *v1, const GLdouble *v2)"<Bar>echoh None<cr>
inorea glRectf glRectf<c-o>:echoh HintHL<Bar>echo "void glRectf(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2)"<Bar>echoh None<cr>
inorea glRectfv glRectfv<c-o>:echoh HintHL<Bar>echo "void glRectfv(const GLfloat *v1, const GLfloat *v2)"<Bar>echoh None<cr>
inorea glRecti glRecti<c-o>:echoh HintHL<Bar>echo "void glRecti(GLint x1, GLint y1, GLint x2, GLint y2)"<Bar>echoh None<cr>
inorea glRectiv glRectiv<c-o>:echoh HintHL<Bar>echo "void glRectiv(const GLint *v1, const GLint *v2)"<Bar>echoh None<cr>
inorea glRects glRects<c-o>:echoh HintHL<Bar>echo "void glRects(GLshort x1, GLshort y1, GLshort x2, GLshort y2)"<Bar>echoh None<cr>
inorea glRectsv glRectsv<c-o>:echoh HintHL<Bar>echo "void glRectsv(const GLshort *v1, const GLshort *v2)"<Bar>echoh None<cr>
inorea glRenderMode glRenderMode<c-o>:echoh HintHL<Bar>echo "GLint glRenderMode(GLenum mode)"<Bar>echoh None<cr>
inorea glRotated glRotated<c-o>:echoh HintHL<Bar>echo "void glRotated(GLdouble angle, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glRotatef glRotatef<c-o>:echoh HintHL<Bar>echo "void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glScaled glScaled<c-o>:echoh HintHL<Bar>echo "void glScaled(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glScalef glScalef<c-o>:echoh HintHL<Bar>echo "void glScalef(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glScissor glScissor<c-o>:echoh HintHL<Bar>echo "void glScissor(GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glSelectBuffer glSelectBuffer<c-o>:echoh HintHL<Bar>echo "void glSelectBuffer(GLsizei size, GLuint *buffer)"<Bar>echoh None<cr>
inorea glShadeModel glShadeModel<c-o>:echoh HintHL<Bar>echo "void glShadeModel(GLenum mode)"<Bar>echoh None<cr>
inorea glStencilFunc glStencilFunc<c-o>:echoh HintHL<Bar>echo "void glStencilFunc(GLenum func, GLint ref, GLuint mask)"<Bar>echoh None<cr>
inorea glStencilMask glStencilMask<c-o>:echoh HintHL<Bar>echo "void glStencilMask(GLuint mask)"<Bar>echoh None<cr>
inorea glStencilOp glStencilOp<c-o>:echoh HintHL<Bar>echo "void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass)"<Bar>echoh None<cr>
inorea glTexCoord1d glTexCoord1d<c-o>:echoh HintHL<Bar>echo "void glTexCoord1d(GLdouble s)"<Bar>echoh None<cr>
inorea glTexCoord1dv glTexCoord1dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord1f glTexCoord1f<c-o>:echoh HintHL<Bar>echo "void glTexCoord1f(GLfloat s)"<Bar>echoh None<cr>
inorea glTexCoord1fv glTexCoord1fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord1i glTexCoord1i<c-o>:echoh HintHL<Bar>echo "void glTexCoord1i(GLint s)"<Bar>echoh None<cr>
inorea glTexCoord1iv glTexCoord1iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord1s glTexCoord1s<c-o>:echoh HintHL<Bar>echo "void glTexCoord1s(GLshort s)"<Bar>echoh None<cr>
inorea glTexCoord1sv glTexCoord1sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glTexCoord2d glTexCoord2d<c-o>:echoh HintHL<Bar>echo "void glTexCoord2d(GLdouble s, GLdouble t)"<Bar>echoh None<cr>
inorea glTexCoord2dv glTexCoord2dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord2f glTexCoord2f<c-o>:echoh HintHL<Bar>echo "void glTexCoord2f(GLfloat s, GLfloat t)"<Bar>echoh None<cr>
inorea glTexCoord2fv glTexCoord2fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord2i glTexCoord2i<c-o>:echoh HintHL<Bar>echo "void glTexCoord2i(GLint s, GLint t)"<Bar>echoh None<cr>
inorea glTexCoord2iv glTexCoord2iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord2s glTexCoord2s<c-o>:echoh HintHL<Bar>echo "void glTexCoord2s(GLshort s, GLshort t)"<Bar>echoh None<cr>
inorea glTexCoord2sv glTexCoord2sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glTexCoord3d glTexCoord3d<c-o>:echoh HintHL<Bar>echo "void glTexCoord3d(GLdouble s, GLdouble t, GLdouble r)"<Bar>echoh None<cr>
inorea glTexCoord3dv glTexCoord3dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord3f glTexCoord3f<c-o>:echoh HintHL<Bar>echo "void glTexCoord3f(GLfloat s, GLfloat t, GLfloat r)"<Bar>echoh None<cr>
inorea glTexCoord3fv glTexCoord3fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord3i glTexCoord3i<c-o>:echoh HintHL<Bar>echo "void glTexCoord3i(GLint s, GLint t, GLint r)"<Bar>echoh None<cr>
inorea glTexCoord3iv glTexCoord3iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord3s glTexCoord3s<c-o>:echoh HintHL<Bar>echo "void glTexCoord3s(GLshort s, GLshort t, GLshort r)"<Bar>echoh None<cr>
inorea glTexCoord3sv glTexCoord3sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glTexCoord4d glTexCoord4d<c-o>:echoh HintHL<Bar>echo "void glTexCoord4d(GLdouble s, GLdouble t, GLdouble r, GLdouble q)"<Bar>echoh None<cr>
inorea glTexCoord4dv glTexCoord4dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord4f glTexCoord4f<c-o>:echoh HintHL<Bar>echo "void glTexCoord4f(GLfloat s, GLfloat t, GLfloat r, GLfloat q)"<Bar>echoh None<cr>
inorea glTexCoord4fv glTexCoord4fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord4i glTexCoord4i<c-o>:echoh HintHL<Bar>echo "void glTexCoord4i(GLint s, GLint t, GLint r, GLint q)"<Bar>echoh None<cr>
inorea glTexCoord4iv glTexCoord4iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord4s glTexCoord4s<c-o>:echoh HintHL<Bar>echo "void glTexCoord4s(GLshort s, GLshort t, GLshort r, GLshort q)"<Bar>echoh None<cr>
inorea glTexCoord4sv glTexCoord4sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glTexCoordPointer glTexCoordPointer<c-o>:echoh HintHL<Bar>echo "void glTexCoordPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glTexEnvf glTexEnvf<c-o>:echoh HintHL<Bar>echo "void glTexEnvf(GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glTexEnvfv glTexEnvfv<c-o>:echoh HintHL<Bar>echo "void glTexEnvfv(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glTexEnvi glTexEnvi<c-o>:echoh HintHL<Bar>echo "void glTexEnvi(GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glTexEnviv glTexEnviv<c-o>:echoh HintHL<Bar>echo "void glTexEnviv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glTexGend glTexGend<c-o>:echoh HintHL<Bar>echo "void glTexGend(GLenum coord, GLenum pname, GLdouble param)"<Bar>echoh None<cr>
inorea glTexGendv glTexGendv<c-o>:echoh HintHL<Bar>echo "void glTexGendv(GLenum coord, GLenum pname, const GLdouble *params)"<Bar>echoh None<cr>
inorea glTexGenf glTexGenf<c-o>:echoh HintHL<Bar>echo "void glTexGenf(GLenum coord, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glTexGenfv glTexGenfv<c-o>:echoh HintHL<Bar>echo "void glTexGenfv(GLenum coord, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glTexGeni glTexGeni<c-o>:echoh HintHL<Bar>echo "void glTexGeni(GLenum coord, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glTexGeniv glTexGeniv<c-o>:echoh HintHL<Bar>echo "void glTexGeniv(GLenum coord, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glTexImage1D glTexImage1D<c-o>:echoh HintHL<Bar>echo "void glTexImage1D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTexImage2D glTexImage2D<c-o>:echoh HintHL<Bar>echo "void glTexImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTexParameterf glTexParameterf<c-o>:echoh HintHL<Bar>echo "void glTexParameterf(GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glTexParameterfv glTexParameterfv<c-o>:echoh HintHL<Bar>echo "void glTexParameterfv(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glTexParameteri glTexParameteri<c-o>:echoh HintHL<Bar>echo "void glTexParameteri(GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glTexParameteriv glTexParameteriv<c-o>:echoh HintHL<Bar>echo "void glTexParameteriv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glTexSubImage1D glTexSubImage1D<c-o>:echoh HintHL<Bar>echo "void glTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTexSubImage2D glTexSubImage2D<c-o>:echoh HintHL<Bar>echo "void glTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTranslated glTranslated<c-o>:echoh HintHL<Bar>echo "void glTranslated(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glTranslatef glTranslatef<c-o>:echoh HintHL<Bar>echo "void glTranslatef(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glVertex2d glVertex2d<c-o>:echoh HintHL<Bar>echo "void glVertex2d(GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertex2dv glVertex2dv<c-o>:echoh HintHL<Bar>echo "void glVertex2dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertex2f glVertex2f<c-o>:echoh HintHL<Bar>echo "void glVertex2f(GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glVertex2fv glVertex2fv<c-o>:echoh HintHL<Bar>echo "void glVertex2fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertex2i glVertex2i<c-o>:echoh HintHL<Bar>echo "void glVertex2i(GLint x, GLint y)"<Bar>echoh None<cr>
inorea glVertex2iv glVertex2iv<c-o>:echoh HintHL<Bar>echo "void glVertex2iv(const GLint *v)"<Bar>echoh None<cr>
inorea glVertex2s glVertex2s<c-o>:echoh HintHL<Bar>echo "void glVertex2s(GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glVertex2sv glVertex2sv<c-o>:echoh HintHL<Bar>echo "void glVertex2sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glVertex3d glVertex3d<c-o>:echoh HintHL<Bar>echo "void glVertex3d(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertex3dv glVertex3dv<c-o>:echoh HintHL<Bar>echo "void glVertex3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertex3f glVertex3f<c-o>:echoh HintHL<Bar>echo "void glVertex3f(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glVertex3fv glVertex3fv<c-o>:echoh HintHL<Bar>echo "void glVertex3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertex3i glVertex3i<c-o>:echoh HintHL<Bar>echo "void glVertex3i(GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glVertex3iv glVertex3iv<c-o>:echoh HintHL<Bar>echo "void glVertex3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glVertex3s glVertex3s<c-o>:echoh HintHL<Bar>echo "void glVertex3s(GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glVertex3sv glVertex3sv<c-o>:echoh HintHL<Bar>echo "void glVertex3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glVertex4d glVertex4d<c-o>:echoh HintHL<Bar>echo "void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertex4dv glVertex4dv<c-o>:echoh HintHL<Bar>echo "void glVertex4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertex4f glVertex4f<c-o>:echoh HintHL<Bar>echo "void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glVertex4fv glVertex4fv<c-o>:echoh HintHL<Bar>echo "void glVertex4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertex4i glVertex4i<c-o>:echoh HintHL<Bar>echo "void glVertex4i(GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glVertex4iv glVertex4iv<c-o>:echoh HintHL<Bar>echo "void glVertex4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glVertex4s glVertex4s<c-o>:echoh HintHL<Bar>echo "void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glVertex4sv glVertex4sv<c-o>:echoh HintHL<Bar>echo "void glVertex4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexPointer glVertexPointer<c-o>:echoh HintHL<Bar>echo "void glVertexPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glViewport glViewport<c-o>:echoh HintHL<Bar>echo "void glViewport(GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glBlendColor glBlendColor<c-o>:echoh HintHL<Bar>echo "void glBlendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"<Bar>echoh None<cr>
inorea glBlendEquation glBlendEquation<c-o>:echoh HintHL<Bar>echo "void glBlendEquation(GLenum mode)"<Bar>echoh None<cr>
inorea glDrawRangeElements glDrawRangeElements<c-o>:echoh HintHL<Bar>echo "void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices)"<Bar>echoh None<cr>
inorea glTexImage3D glTexImage3D<c-o>:echoh HintHL<Bar>echo "void glTexImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTexSubImage3D glTexSubImage3D<c-o>:echoh HintHL<Bar>echo "void glTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glCopyTexSubImage3D glCopyTexSubImage3D<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glColorTable glColorTable<c-o>:echoh HintHL<Bar>echo "void glColorTable(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *table)"<Bar>echoh None<cr>
inorea glColorTableParameterfv glColorTableParameterfv<c-o>:echoh HintHL<Bar>echo "void glColorTableParameterfv(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glColorTableParameteriv glColorTableParameteriv<c-o>:echoh HintHL<Bar>echo "void glColorTableParameteriv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glCopyColorTable glCopyColorTable<c-o>:echoh HintHL<Bar>echo "void glCopyColorTable(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glGetColorTable glGetColorTable<c-o>:echoh HintHL<Bar>echo "void glGetColorTable(GLenum target, GLenum format, GLenum type, GLvoid *table)"<Bar>echoh None<cr>
inorea glGetColorTableParameterfv glGetColorTableParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetColorTableParameteriv glGetColorTableParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glColorSubTable glColorSubTable<c-o>:echoh HintHL<Bar>echo "void glColorSubTable(GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCopyColorSubTable glCopyColorSubTable<c-o>:echoh HintHL<Bar>echo "void glCopyColorSubTable(GLenum target, GLsizei start, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glConvolutionFilter1D glConvolutionFilter1D<c-o>:echoh HintHL<Bar>echo "void glConvolutionFilter1D(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *image)"<Bar>echoh None<cr>
inorea glConvolutionFilter2D glConvolutionFilter2D<c-o>:echoh HintHL<Bar>echo "void glConvolutionFilter2D(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *image)"<Bar>echoh None<cr>
inorea glConvolutionParameterf glConvolutionParameterf<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameterf(GLenum target, GLenum pname, GLfloat params)"<Bar>echoh None<cr>
inorea glConvolutionParameterfv glConvolutionParameterfv<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameterfv(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glConvolutionParameteri glConvolutionParameteri<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameteri(GLenum target, GLenum pname, GLint params)"<Bar>echoh None<cr>
inorea glConvolutionParameteriv glConvolutionParameteriv<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameteriv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glCopyConvolutionFilter1D glCopyConvolutionFilter1D<c-o>:echoh HintHL<Bar>echo "void glCopyConvolutionFilter1D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyConvolutionFilter2D glCopyConvolutionFilter2D<c-o>:echoh HintHL<Bar>echo "void glCopyConvolutionFilter2D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetConvolutionFilter glGetConvolutionFilter<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionFilter(GLenum target, GLenum format, GLenum type, GLvoid *image)"<Bar>echoh None<cr>
inorea glGetConvolutionParameterfv glGetConvolutionParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetConvolutionParameteriv glGetConvolutionParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetSeparableFilter glGetSeparableFilter<c-o>:echoh HintHL<Bar>echo "void glGetSeparableFilter(GLenum target, GLenum format, GLenum type, GLvoid *row, GLvoid *column, GLvoid *span)"<Bar>echoh None<cr>
inorea glSeparableFilter2D glSeparableFilter2D<c-o>:echoh HintHL<Bar>echo "void glSeparableFilter2D(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *row, const GLvoid *column)"<Bar>echoh None<cr>
inorea glGetHistogram glGetHistogram<c-o>:echoh HintHL<Bar>echo "void glGetHistogram(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetHistogramParameterfv glGetHistogramParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetHistogramParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetHistogramParameteriv glGetHistogramParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMinmax glGetMinmax<c-o>:echoh HintHL<Bar>echo "void glGetMinmax(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetMinmaxParameterfv glGetMinmaxParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetMinmaxParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMinmaxParameteriv glGetMinmaxParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glHistogram glHistogram<c-o>:echoh HintHL<Bar>echo "void glHistogram(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink)"<Bar>echoh None<cr>
inorea glMinmax glMinmax<c-o>:echoh HintHL<Bar>echo "void glMinmax(GLenum target, GLenum internalformat, GLboolean sink)"<Bar>echoh None<cr>
inorea glResetHistogram glResetHistogram<c-o>:echoh HintHL<Bar>echo "void glResetHistogram(GLenum target)"<Bar>echoh None<cr>
inorea glResetMinmax glResetMinmax<c-o>:echoh HintHL<Bar>echo "void glResetMinmax(GLenum target)"<Bar>echoh None<cr>
inorea glActiveTexture glActiveTexture<c-o>:echoh HintHL<Bar>echo "void glActiveTexture(GLenum texture)"<Bar>echoh None<cr>
inorea glSampleCoverage glSampleCoverage<c-o>:echoh HintHL<Bar>echo "void glSampleCoverage(GLclampf value, GLboolean invert)"<Bar>echoh None<cr>
inorea glCompressedTexImage3D glCompressedTexImage3D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexImage2D glCompressedTexImage2D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexImage1D glCompressedTexImage1D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage1D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage3D glCompressedTexSubImage3D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage2D glCompressedTexSubImage2D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage1D glCompressedTexSubImage1D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glGetCompressedTexImage glGetCompressedTexImage<c-o>:echoh HintHL<Bar>echo "void glGetCompressedTexImage(GLenum target, GLint level, GLvoid *img)"<Bar>echoh None<cr>
inorea glClientActiveTexture glClientActiveTexture<c-o>:echoh HintHL<Bar>echo "void glClientActiveTexture(GLenum texture)"<Bar>echoh None<cr>
inorea glMultiTexCoord1d glMultiTexCoord1d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1d(GLenum target, GLdouble s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1dv glMultiTexCoord1dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1f glMultiTexCoord1f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1f(GLenum target, GLfloat s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1fv glMultiTexCoord1fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1i glMultiTexCoord1i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1i(GLenum target, GLint s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1iv glMultiTexCoord1iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1s glMultiTexCoord1s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1s(GLenum target, GLshort s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1sv glMultiTexCoord1sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2d glMultiTexCoord2d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2d(GLenum target, GLdouble s, GLdouble t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2dv glMultiTexCoord2dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2f glMultiTexCoord2f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2f(GLenum target, GLfloat s, GLfloat t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2fv glMultiTexCoord2fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2i glMultiTexCoord2i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2i(GLenum target, GLint s, GLint t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2iv glMultiTexCoord2iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2s glMultiTexCoord2s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2s(GLenum target, GLshort s, GLshort t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2sv glMultiTexCoord2sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3d glMultiTexCoord3d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3d(GLenum target, GLdouble s, GLdouble t, GLdouble r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3dv glMultiTexCoord3dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3f glMultiTexCoord3f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3f(GLenum target, GLfloat s, GLfloat t, GLfloat r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3fv glMultiTexCoord3fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3i glMultiTexCoord3i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3i(GLenum target, GLint s, GLint t, GLint r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3iv glMultiTexCoord3iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3s glMultiTexCoord3s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3s(GLenum target, GLshort s, GLshort t, GLshort r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3sv glMultiTexCoord3sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4d glMultiTexCoord4d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4d(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4dv glMultiTexCoord4dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4f glMultiTexCoord4f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4f(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4fv glMultiTexCoord4fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4i glMultiTexCoord4i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4i(GLenum target, GLint s, GLint t, GLint r, GLint q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4iv glMultiTexCoord4iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4s glMultiTexCoord4s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4s(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4sv glMultiTexCoord4sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glLoadTransposeMatrixf glLoadTransposeMatrixf<c-o>:echoh HintHL<Bar>echo "void glLoadTransposeMatrixf(const GLfloat *m)"<Bar>echoh None<cr>
inorea glLoadTransposeMatrixd glLoadTransposeMatrixd<c-o>:echoh HintHL<Bar>echo "void glLoadTransposeMatrixd(const GLdouble *m)"<Bar>echoh None<cr>
inorea glMultTransposeMatrixf glMultTransposeMatrixf<c-o>:echoh HintHL<Bar>echo "void glMultTransposeMatrixf(const GLfloat *m)"<Bar>echoh None<cr>
inorea glMultTransposeMatrixd glMultTransposeMatrixd<c-o>:echoh HintHL<Bar>echo "void glMultTransposeMatrixd(const GLdouble *m)"<Bar>echoh None<cr>
inorea glBlendFuncSeparate glBlendFuncSeparate<c-o>:echoh HintHL<Bar>echo "void glBlendFuncSeparate(GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha)"<Bar>echoh None<cr>
inorea glMultiDrawArrays glMultiDrawArrays<c-o>:echoh HintHL<Bar>echo "void glMultiDrawArrays(GLenum mode, const GLint *first, const GLsizei *count, GLsizei primcount)"<Bar>echoh None<cr>
inorea glMultiDrawElements glMultiDrawElements<c-o>:echoh HintHL<Bar>echo "void glMultiDrawElements(GLenum mode, const GLsizei *count, GLenum type, const GLvoid* *indices, GLsizei primcount)"<Bar>echoh None<cr>
inorea glPointParameterf glPointParameterf<c-o>:echoh HintHL<Bar>echo "void glPointParameterf(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPointParameterfv glPointParameterfv<c-o>:echoh HintHL<Bar>echo "void glPointParameterfv(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glPointParameteri glPointParameteri<c-o>:echoh HintHL<Bar>echo "void glPointParameteri(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPointParameteriv glPointParameteriv<c-o>:echoh HintHL<Bar>echo "void glPointParameteriv(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glFogCoordf glFogCoordf<c-o>:echoh HintHL<Bar>echo "void glFogCoordf(GLfloat coord)"<Bar>echoh None<cr>
inorea glFogCoordfv glFogCoordfv<c-o>:echoh HintHL<Bar>echo "void glFogCoordfv(const GLfloat *coord)"<Bar>echoh None<cr>
inorea glFogCoordd glFogCoordd<c-o>:echoh HintHL<Bar>echo "void glFogCoordd(GLdouble coord)"<Bar>echoh None<cr>
inorea glFogCoorddv glFogCoorddv<c-o>:echoh HintHL<Bar>echo "void glFogCoorddv(const GLdouble *coord)"<Bar>echoh None<cr>
inorea glFogCoordPointer glFogCoordPointer<c-o>:echoh HintHL<Bar>echo "void glFogCoordPointer(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glSecondaryColor3b glSecondaryColor3b<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3b(GLbyte red, GLbyte green, GLbyte blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3bv glSecondaryColor3bv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3bv(const GLbyte *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3d glSecondaryColor3d<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3d(GLdouble red, GLdouble green, GLdouble blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3dv glSecondaryColor3dv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3f glSecondaryColor3f<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3f(GLfloat red, GLfloat green, GLfloat blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3fv glSecondaryColor3fv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3i glSecondaryColor3i<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3i(GLint red, GLint green, GLint blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3iv glSecondaryColor3iv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3s glSecondaryColor3s<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3s(GLshort red, GLshort green, GLshort blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3sv glSecondaryColor3sv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3ub glSecondaryColor3ub<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3ub(GLubyte red, GLubyte green, GLubyte blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3ubv glSecondaryColor3ubv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3ubv(const GLubyte *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3ui glSecondaryColor3ui<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3ui(GLuint red, GLuint green, GLuint blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3uiv glSecondaryColor3uiv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3uiv(const GLuint *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3us glSecondaryColor3us<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3us(GLushort red, GLushort green, GLushort blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3usv glSecondaryColor3usv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3usv(const GLushort *v)"<Bar>echoh None<cr>
inorea glSecondaryColorPointer glSecondaryColorPointer<c-o>:echoh HintHL<Bar>echo "void glSecondaryColorPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glWindowPos2d glWindowPos2d<c-o>:echoh HintHL<Bar>echo "void glWindowPos2d(GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glWindowPos2dv glWindowPos2dv<c-o>:echoh HintHL<Bar>echo "void glWindowPos2dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glWindowPos2f glWindowPos2f<c-o>:echoh HintHL<Bar>echo "void glWindowPos2f(GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glWindowPos2fv glWindowPos2fv<c-o>:echoh HintHL<Bar>echo "void glWindowPos2fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glWindowPos2i glWindowPos2i<c-o>:echoh HintHL<Bar>echo "void glWindowPos2i(GLint x, GLint y)"<Bar>echoh None<cr>
inorea glWindowPos2iv glWindowPos2iv<c-o>:echoh HintHL<Bar>echo "void glWindowPos2iv(const GLint *v)"<Bar>echoh None<cr>
inorea glWindowPos2s glWindowPos2s<c-o>:echoh HintHL<Bar>echo "void glWindowPos2s(GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glWindowPos2sv glWindowPos2sv<c-o>:echoh HintHL<Bar>echo "void glWindowPos2sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glWindowPos3d glWindowPos3d<c-o>:echoh HintHL<Bar>echo "void glWindowPos3d(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glWindowPos3dv glWindowPos3dv<c-o>:echoh HintHL<Bar>echo "void glWindowPos3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glWindowPos3f glWindowPos3f<c-o>:echoh HintHL<Bar>echo "void glWindowPos3f(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glWindowPos3fv glWindowPos3fv<c-o>:echoh HintHL<Bar>echo "void glWindowPos3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glWindowPos3i glWindowPos3i<c-o>:echoh HintHL<Bar>echo "void glWindowPos3i(GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glWindowPos3iv glWindowPos3iv<c-o>:echoh HintHL<Bar>echo "void glWindowPos3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glWindowPos3s glWindowPos3s<c-o>:echoh HintHL<Bar>echo "void glWindowPos3s(GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glWindowPos3sv glWindowPos3sv<c-o>:echoh HintHL<Bar>echo "void glWindowPos3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glGenQueries glGenQueries<c-o>:echoh HintHL<Bar>echo "void glGenQueries(GLsizei n, GLuint *ids)"<Bar>echoh None<cr>
inorea glDeleteQueries glDeleteQueries<c-o>:echoh HintHL<Bar>echo "void glDeleteQueries(GLsizei n, const GLuint *ids)"<Bar>echoh None<cr>
inorea glIsQuery glIsQuery<c-o>:echoh HintHL<Bar>echo "GLboolean glIsQuery(GLuint id)"<Bar>echoh None<cr>
inorea glBeginQuery glBeginQuery<c-o>:echoh HintHL<Bar>echo "void glBeginQuery(GLenum target, GLuint id)"<Bar>echoh None<cr>
inorea glEndQuery glEndQuery<c-o>:echoh HintHL<Bar>echo "void glEndQuery(GLenum target)"<Bar>echoh None<cr>
inorea glGetQueryiv glGetQueryiv<c-o>:echoh HintHL<Bar>echo "void glGetQueryiv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetQueryObjectiv glGetQueryObjectiv<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjectiv(GLuint id, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetQueryObjectuiv glGetQueryObjectuiv<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjectuiv(GLuint id, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glBindBuffer glBindBuffer<c-o>:echoh HintHL<Bar>echo "void glBindBuffer(GLenum target, GLuint buffer)"<Bar>echoh None<cr>
inorea glDeleteBuffers glDeleteBuffers<c-o>:echoh HintHL<Bar>echo "void glDeleteBuffers(GLsizei n, const GLuint *buffers)"<Bar>echoh None<cr>
inorea glGenBuffers glGenBuffers<c-o>:echoh HintHL<Bar>echo "void glGenBuffers(GLsizei n, GLuint *buffers)"<Bar>echoh None<cr>
inorea glIsBuffer glIsBuffer<c-o>:echoh HintHL<Bar>echo "GLboolean glIsBuffer(GLuint buffer)"<Bar>echoh None<cr>
inorea glBufferData glBufferData<c-o>:echoh HintHL<Bar>echo "void glBufferData(GLenum target, GLsizeiptr size, const GLvoid *data, GLenum usage)"<Bar>echoh None<cr>
inorea glBufferSubData glBufferSubData<c-o>:echoh HintHL<Bar>echo "void glBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, const GLvoid *data)"<Bar>echoh None<cr>
inorea glGetBufferSubData glGetBufferSubData<c-o>:echoh HintHL<Bar>echo "void glGetBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, GLvoid *data)"<Bar>echoh None<cr>
inorea glMapBuffer glMapBuffer<c-o>:echoh HintHL<Bar>echo "GLvoid *glMapBuffer(GLenum target, GLenum access)"<Bar>echoh None<cr>
inorea glUnmapBuffer glUnmapBuffer<c-o>:echoh HintHL<Bar>echo "GLboolean glUnmapBuffer(GLenum target)"<Bar>echoh None<cr>
inorea glGetBufferParameteriv glGetBufferParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetBufferParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetBufferPointerv glGetBufferPointerv<c-o>:echoh HintHL<Bar>echo "void glGetBufferPointerv(GLenum target, GLenum pname, GLvoid* *params)"<Bar>echoh None<cr>
inorea glBlendEquationSeparate glBlendEquationSeparate<c-o>:echoh HintHL<Bar>echo "void glBlendEquationSeparate(GLenum modeRGB, GLenum modeAlpha)"<Bar>echoh None<cr>
inorea glDrawBuffers glDrawBuffers<c-o>:echoh HintHL<Bar>echo "void glDrawBuffers(GLsizei n, const GLenum *bufs)"<Bar>echoh None<cr>
inorea glStencilOpSeparate glStencilOpSeparate<c-o>:echoh HintHL<Bar>echo "void glStencilOpSeparate(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass)"<Bar>echoh None<cr>
inorea glStencilFuncSeparate glStencilFuncSeparate<c-o>:echoh HintHL<Bar>echo "void glStencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask)"<Bar>echoh None<cr>
inorea glStencilMaskSeparate glStencilMaskSeparate<c-o>:echoh HintHL<Bar>echo "void glStencilMaskSeparate(GLenum face, GLuint mask)"<Bar>echoh None<cr>
inorea glAttachShader glAttachShader<c-o>:echoh HintHL<Bar>echo "void glAttachShader(GLuint program, GLuint shader)"<Bar>echoh None<cr>
inorea glBindAttribLocation glBindAttribLocation<c-o>:echoh HintHL<Bar>echo "void glBindAttribLocation(GLuint program, GLuint index, const GLchar *name)"<Bar>echoh None<cr>
inorea glCompileShader glCompileShader<c-o>:echoh HintHL<Bar>echo "void glCompileShader(GLuint shader)"<Bar>echoh None<cr>
inorea glCreateProgram glCreateProgram<c-o>:echoh HintHL<Bar>echo "GLuint glCreateProgram(void)"<Bar>echoh None<cr>
inorea glCreateShader glCreateShader<c-o>:echoh HintHL<Bar>echo "GLuint glCreateShader(GLenum type)"<Bar>echoh None<cr>
inorea glDeleteProgram glDeleteProgram<c-o>:echoh HintHL<Bar>echo "void glDeleteProgram(GLuint program)"<Bar>echoh None<cr>
inorea glDeleteShader glDeleteShader<c-o>:echoh HintHL<Bar>echo "void glDeleteShader(GLuint shader)"<Bar>echoh None<cr>
inorea glDetachShader glDetachShader<c-o>:echoh HintHL<Bar>echo "void glDetachShader(GLuint program, GLuint shader)"<Bar>echoh None<cr>
inorea glDisableVertexAttribArray glDisableVertexAttribArray<c-o>:echoh HintHL<Bar>echo "void glDisableVertexAttribArray(GLuint index)"<Bar>echoh None<cr>
inorea glEnableVertexAttribArray glEnableVertexAttribArray<c-o>:echoh HintHL<Bar>echo "void glEnableVertexAttribArray(GLuint index)"<Bar>echoh None<cr>
inorea glGetActiveAttrib glGetActiveAttrib<c-o>:echoh HintHL<Bar>echo "void glGetActiveAttrib(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name)"<Bar>echoh None<cr>
inorea glGetActiveUniform glGetActiveUniform<c-o>:echoh HintHL<Bar>echo "void glGetActiveUniform(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name)"<Bar>echoh None<cr>
inorea glGetAttachedShaders glGetAttachedShaders<c-o>:echoh HintHL<Bar>echo "void glGetAttachedShaders(GLuint program, GLsizei maxCount, GLsizei *count, GLuint *obj)"<Bar>echoh None<cr>
inorea glGetAttribLocation glGetAttribLocation<c-o>:echoh HintHL<Bar>echo "GLint glGetAttribLocation(GLuint program, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetProgramiv glGetProgramiv<c-o>:echoh HintHL<Bar>echo "void glGetProgramiv(GLuint program, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetProgramInfoLog glGetProgramInfoLog<c-o>:echoh HintHL<Bar>echo "void glGetProgramInfoLog(GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog)"<Bar>echoh None<cr>
inorea glGetShaderiv glGetShaderiv<c-o>:echoh HintHL<Bar>echo "void glGetShaderiv(GLuint shader, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetShaderInfoLog glGetShaderInfoLog<c-o>:echoh HintHL<Bar>echo "void glGetShaderInfoLog(GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog)"<Bar>echoh None<cr>
inorea glGetShaderSource glGetShaderSource<c-o>:echoh HintHL<Bar>echo "void glGetShaderSource(GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *source)"<Bar>echoh None<cr>
inorea glGetUniformLocation glGetUniformLocation<c-o>:echoh HintHL<Bar>echo "GLint glGetUniformLocation(GLuint program, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetUniformfv glGetUniformfv<c-o>:echoh HintHL<Bar>echo "void glGetUniformfv(GLuint program, GLint location, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetUniformiv glGetUniformiv<c-o>:echoh HintHL<Bar>echo "void glGetUniformiv(GLuint program, GLint location, GLint *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribdv glGetVertexAttribdv<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribdv(GLuint index, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribfv glGetVertexAttribfv<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribfv(GLuint index, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribiv glGetVertexAttribiv<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribiv(GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribPointerv glGetVertexAttribPointerv<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribPointerv(GLuint index, GLenum pname, GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glIsProgram glIsProgram<c-o>:echoh HintHL<Bar>echo "GLboolean glIsProgram(GLuint program)"<Bar>echoh None<cr>
inorea glIsShader glIsShader<c-o>:echoh HintHL<Bar>echo "GLboolean glIsShader(GLuint shader)"<Bar>echoh None<cr>
inorea glLinkProgram glLinkProgram<c-o>:echoh HintHL<Bar>echo "void glLinkProgram(GLuint program)"<Bar>echoh None<cr>
inorea glShaderSource glShaderSource<c-o>:echoh HintHL<Bar>echo "void glShaderSource(GLuint shader, GLsizei count, const GLchar* *string, const GLint *length)"<Bar>echoh None<cr>
inorea glUseProgram glUseProgram<c-o>:echoh HintHL<Bar>echo "void glUseProgram(GLuint program)"<Bar>echoh None<cr>
inorea glUniform1f glUniform1f<c-o>:echoh HintHL<Bar>echo "void glUniform1f(GLint location, GLfloat v0)"<Bar>echoh None<cr>
inorea glUniform2f glUniform2f<c-o>:echoh HintHL<Bar>echo "void glUniform2f(GLint location, GLfloat v0, GLfloat v1)"<Bar>echoh None<cr>
inorea glUniform3f glUniform3f<c-o>:echoh HintHL<Bar>echo "void glUniform3f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2)"<Bar>echoh None<cr>
inorea glUniform4f glUniform4f<c-o>:echoh HintHL<Bar>echo "void glUniform4f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)"<Bar>echoh None<cr>
inorea glUniform1i glUniform1i<c-o>:echoh HintHL<Bar>echo "void glUniform1i(GLint location, GLint v0)"<Bar>echoh None<cr>
inorea glUniform2i glUniform2i<c-o>:echoh HintHL<Bar>echo "void glUniform2i(GLint location, GLint v0, GLint v1)"<Bar>echoh None<cr>
inorea glUniform3i glUniform3i<c-o>:echoh HintHL<Bar>echo "void glUniform3i(GLint location, GLint v0, GLint v1, GLint v2)"<Bar>echoh None<cr>
inorea glUniform4i glUniform4i<c-o>:echoh HintHL<Bar>echo "void glUniform4i(GLint location, GLint v0, GLint v1, GLint v2, GLint v3)"<Bar>echoh None<cr>
inorea glUniform1fv glUniform1fv<c-o>:echoh HintHL<Bar>echo "void glUniform1fv(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform2fv glUniform2fv<c-o>:echoh HintHL<Bar>echo "void glUniform2fv(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform3fv glUniform3fv<c-o>:echoh HintHL<Bar>echo "void glUniform3fv(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform4fv glUniform4fv<c-o>:echoh HintHL<Bar>echo "void glUniform4fv(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform1iv glUniform1iv<c-o>:echoh HintHL<Bar>echo "void glUniform1iv(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniform2iv glUniform2iv<c-o>:echoh HintHL<Bar>echo "void glUniform2iv(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniform3iv glUniform3iv<c-o>:echoh HintHL<Bar>echo "void glUniform3iv(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniform4iv glUniform4iv<c-o>:echoh HintHL<Bar>echo "void glUniform4iv(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniformMatrix2fv glUniformMatrix2fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix2fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix3fv glUniformMatrix3fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix3fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix4fv glUniformMatrix4fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix4fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glValidateProgram glValidateProgram<c-o>:echoh HintHL<Bar>echo "void glValidateProgram(GLuint program)"<Bar>echoh None<cr>
inorea glVertexAttrib1d glVertexAttrib1d<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1d(GLuint index, GLdouble x)"<Bar>echoh None<cr>
inorea glVertexAttrib1dv glVertexAttrib1dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib1f glVertexAttrib1f<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1f(GLuint index, GLfloat x)"<Bar>echoh None<cr>
inorea glVertexAttrib1fv glVertexAttrib1fv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1fv(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib1s glVertexAttrib1s<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1s(GLuint index, GLshort x)"<Bar>echoh None<cr>
inorea glVertexAttrib1sv glVertexAttrib1sv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1sv(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2d glVertexAttrib2d<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2d(GLuint index, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertexAttrib2dv glVertexAttrib2dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2f glVertexAttrib2f<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2f(GLuint index, GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glVertexAttrib2fv glVertexAttrib2fv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2fv(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2s glVertexAttrib2s<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2s(GLuint index, GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glVertexAttrib2sv glVertexAttrib2sv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2sv(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3d glVertexAttrib3d<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3d(GLuint index, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertexAttrib3dv glVertexAttrib3dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3f glVertexAttrib3f<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3f(GLuint index, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glVertexAttrib3fv glVertexAttrib3fv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3fv(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3s glVertexAttrib3s<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3s(GLuint index, GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glVertexAttrib3sv glVertexAttrib3sv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3sv(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4Nbv glVertexAttrib4Nbv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4Nbv(GLuint index, const GLbyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4Niv glVertexAttrib4Niv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4Niv(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4Nsv glVertexAttrib4Nsv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4Nsv(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4Nub glVertexAttrib4Nub<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4Nub(GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w)"<Bar>echoh None<cr>
inorea glVertexAttrib4Nubv glVertexAttrib4Nubv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4Nubv(GLuint index, const GLubyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4Nuiv glVertexAttrib4Nuiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4Nuiv(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4Nusv glVertexAttrib4Nusv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4Nusv(GLuint index, const GLushort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4bv glVertexAttrib4bv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4bv(GLuint index, const GLbyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4d glVertexAttrib4d<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4d(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertexAttrib4dv glVertexAttrib4dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4f glVertexAttrib4f<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4f(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glVertexAttrib4fv glVertexAttrib4fv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4fv(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4iv glVertexAttrib4iv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4iv(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4s glVertexAttrib4s<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4s(GLuint index, GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glVertexAttrib4sv glVertexAttrib4sv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4sv(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4ubv glVertexAttrib4ubv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4ubv(GLuint index, const GLubyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4uiv glVertexAttrib4uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4uiv(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4usv glVertexAttrib4usv<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4usv(GLuint index, const GLushort *v)"<Bar>echoh None<cr>
inorea glVertexAttribPointer glVertexAttribPointer<c-o>:echoh HintHL<Bar>echo "void glVertexAttribPointer(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glUniformMatrix2x3fv glUniformMatrix2x3fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix2x3fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix3x2fv glUniformMatrix3x2fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix3x2fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix2x4fv glUniformMatrix2x4fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix2x4fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix4x2fv glUniformMatrix4x2fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix4x2fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix3x4fv glUniformMatrix3x4fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix3x4fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix4x3fv glUniformMatrix4x3fv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix4x3fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glColorMaski glColorMaski<c-o>:echoh HintHL<Bar>echo "void glColorMaski(GLuint index, GLboolean r, GLboolean g, GLboolean b, GLboolean a)"<Bar>echoh None<cr>
inorea glGetBooleani_v glGetBooleani_v<c-o>:echoh HintHL<Bar>echo "void glGetBooleani_v(GLenum target, GLuint index, GLboolean *data)"<Bar>echoh None<cr>
inorea glGetIntegeri_v glGetIntegeri_v<c-o>:echoh HintHL<Bar>echo "void glGetIntegeri_v(GLenum target, GLuint index, GLint *data)"<Bar>echoh None<cr>
inorea glEnablei glEnablei<c-o>:echoh HintHL<Bar>echo "void glEnablei(GLenum target, GLuint index)"<Bar>echoh None<cr>
inorea glDisablei glDisablei<c-o>:echoh HintHL<Bar>echo "void glDisablei(GLenum target, GLuint index)"<Bar>echoh None<cr>
inorea glIsEnabledi glIsEnabledi<c-o>:echoh HintHL<Bar>echo "GLboolean glIsEnabledi(GLenum target, GLuint index)"<Bar>echoh None<cr>
inorea glBeginTransformFeedback glBeginTransformFeedback<c-o>:echoh HintHL<Bar>echo "void glBeginTransformFeedback(GLenum primitiveMode)"<Bar>echoh None<cr>
inorea glEndTransformFeedback glEndTransformFeedback<c-o>:echoh HintHL<Bar>echo "void glEndTransformFeedback(void)"<Bar>echoh None<cr>
inorea glBindBufferRange glBindBufferRange<c-o>:echoh HintHL<Bar>echo "void glBindBufferRange(GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size)"<Bar>echoh None<cr>
inorea glBindBufferBase glBindBufferBase<c-o>:echoh HintHL<Bar>echo "void glBindBufferBase(GLenum target, GLuint index, GLuint buffer)"<Bar>echoh None<cr>
inorea glTransformFeedbackVaryings glTransformFeedbackVaryings<c-o>:echoh HintHL<Bar>echo "void glTransformFeedbackVaryings(GLuint program, GLsizei count, const GLchar* *varyings, GLenum bufferMode)"<Bar>echoh None<cr>
inorea glGetTransformFeedbackVarying glGetTransformFeedbackVarying<c-o>:echoh HintHL<Bar>echo "void glGetTransformFeedbackVarying(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLsizei *size, GLenum *type, GLchar *name)"<Bar>echoh None<cr>
inorea glClampColor glClampColor<c-o>:echoh HintHL<Bar>echo "void glClampColor(GLenum target, GLenum clamp)"<Bar>echoh None<cr>
inorea glBeginConditionalRender glBeginConditionalRender<c-o>:echoh HintHL<Bar>echo "void glBeginConditionalRender(GLuint id, GLenum mode)"<Bar>echoh None<cr>
inorea glEndConditionalRender glEndConditionalRender<c-o>:echoh HintHL<Bar>echo "void glEndConditionalRender(void)"<Bar>echoh None<cr>
inorea glVertexAttribIPointer glVertexAttribIPointer<c-o>:echoh HintHL<Bar>echo "void glVertexAttribIPointer(GLuint index, GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glGetVertexAttribIiv glGetVertexAttribIiv<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribIiv(GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribIuiv glGetVertexAttribIuiv<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribIuiv(GLuint index, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glVertexAttribI1i glVertexAttribI1i<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1i(GLuint index, GLint x)"<Bar>echoh None<cr>
inorea glVertexAttribI2i glVertexAttribI2i<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2i(GLuint index, GLint x, GLint y)"<Bar>echoh None<cr>
inorea glVertexAttribI3i glVertexAttribI3i<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3i(GLuint index, GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glVertexAttribI4i glVertexAttribI4i<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4i(GLuint index, GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glVertexAttribI1ui glVertexAttribI1ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1ui(GLuint index, GLuint x)"<Bar>echoh None<cr>
inorea glVertexAttribI2ui glVertexAttribI2ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2ui(GLuint index, GLuint x, GLuint y)"<Bar>echoh None<cr>
inorea glVertexAttribI3ui glVertexAttribI3ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3ui(GLuint index, GLuint x, GLuint y, GLuint z)"<Bar>echoh None<cr>
inorea glVertexAttribI4ui glVertexAttribI4ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4ui(GLuint index, GLuint x, GLuint y, GLuint z, GLuint w)"<Bar>echoh None<cr>
inorea glVertexAttribI1iv glVertexAttribI1iv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1iv(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI2iv glVertexAttribI2iv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2iv(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI3iv glVertexAttribI3iv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3iv(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4iv glVertexAttribI4iv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4iv(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI1uiv glVertexAttribI1uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1uiv(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI2uiv glVertexAttribI2uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2uiv(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI3uiv glVertexAttribI3uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3uiv(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4uiv glVertexAttribI4uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4uiv(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4bv glVertexAttribI4bv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4bv(GLuint index, const GLbyte *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4sv glVertexAttribI4sv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4sv(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4ubv glVertexAttribI4ubv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4ubv(GLuint index, const GLubyte *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4usv glVertexAttribI4usv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4usv(GLuint index, const GLushort *v)"<Bar>echoh None<cr>
inorea glGetUniformuiv glGetUniformuiv<c-o>:echoh HintHL<Bar>echo "void glGetUniformuiv(GLuint program, GLint location, GLuint *params)"<Bar>echoh None<cr>
inorea glBindFragDataLocation glBindFragDataLocation<c-o>:echoh HintHL<Bar>echo "void glBindFragDataLocation(GLuint program, GLuint color, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetFragDataLocation glGetFragDataLocation<c-o>:echoh HintHL<Bar>echo "GLint glGetFragDataLocation(GLuint program, const GLchar *name)"<Bar>echoh None<cr>
inorea glUniform1ui glUniform1ui<c-o>:echoh HintHL<Bar>echo "void glUniform1ui(GLint location, GLuint v0)"<Bar>echoh None<cr>
inorea glUniform2ui glUniform2ui<c-o>:echoh HintHL<Bar>echo "void glUniform2ui(GLint location, GLuint v0, GLuint v1)"<Bar>echoh None<cr>
inorea glUniform3ui glUniform3ui<c-o>:echoh HintHL<Bar>echo "void glUniform3ui(GLint location, GLuint v0, GLuint v1, GLuint v2)"<Bar>echoh None<cr>
inorea glUniform4ui glUniform4ui<c-o>:echoh HintHL<Bar>echo "void glUniform4ui(GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3)"<Bar>echoh None<cr>
inorea glUniform1uiv glUniform1uiv<c-o>:echoh HintHL<Bar>echo "void glUniform1uiv(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glUniform2uiv glUniform2uiv<c-o>:echoh HintHL<Bar>echo "void glUniform2uiv(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glUniform3uiv glUniform3uiv<c-o>:echoh HintHL<Bar>echo "void glUniform3uiv(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glUniform4uiv glUniform4uiv<c-o>:echoh HintHL<Bar>echo "void glUniform4uiv(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glTexParameterIiv glTexParameterIiv<c-o>:echoh HintHL<Bar>echo "void glTexParameterIiv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glTexParameterIuiv glTexParameterIuiv<c-o>:echoh HintHL<Bar>echo "void glTexParameterIuiv(GLenum target, GLenum pname, const GLuint *params)"<Bar>echoh None<cr>
inorea glGetTexParameterIiv glGetTexParameterIiv<c-o>:echoh HintHL<Bar>echo "void glGetTexParameterIiv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTexParameterIuiv glGetTexParameterIuiv<c-o>:echoh HintHL<Bar>echo "void glGetTexParameterIuiv(GLenum target, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glClearBufferiv glClearBufferiv<c-o>:echoh HintHL<Bar>echo "void glClearBufferiv(GLenum buffer, GLint drawbuffer, const GLint *value)"<Bar>echoh None<cr>
inorea glClearBufferuiv glClearBufferuiv<c-o>:echoh HintHL<Bar>echo "void glClearBufferuiv(GLenum buffer, GLint drawbuffer, const GLuint *value)"<Bar>echoh None<cr>
inorea glClearBufferfv glClearBufferfv<c-o>:echoh HintHL<Bar>echo "void glClearBufferfv(GLenum buffer, GLint drawbuffer, const GLfloat *value)"<Bar>echoh None<cr>
inorea glClearBufferfi glClearBufferfi<c-o>:echoh HintHL<Bar>echo "void glClearBufferfi(GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil)"<Bar>echoh None<cr>
inorea glGetStringi glGetStringi<c-o>:echoh HintHL<Bar>echo "const GLubyte *glGetStringi(GLenum name, GLuint index)"<Bar>echoh None<cr>
inorea glDrawArraysInstanced glDrawArraysInstanced<c-o>:echoh HintHL<Bar>echo "void glDrawArraysInstanced(GLenum mode, GLint first, GLsizei count, GLsizei primcount)"<Bar>echoh None<cr>
inorea glDrawElementsInstanced glDrawElementsInstanced<c-o>:echoh HintHL<Bar>echo "void glDrawElementsInstanced(GLenum mode, GLsizei count, GLenum type, const GLvoid *indices, GLsizei primcount)"<Bar>echoh None<cr>
inorea glTexBuffer glTexBuffer<c-o>:echoh HintHL<Bar>echo "void glTexBuffer(GLenum target, GLenum internalformat, GLuint buffer)"<Bar>echoh None<cr>
inorea glPrimitiveRestartIndex glPrimitiveRestartIndex<c-o>:echoh HintHL<Bar>echo "void glPrimitiveRestartIndex(GLuint index)"<Bar>echoh None<cr>
inorea glGetInteger64i_v glGetInteger64i_v<c-o>:echoh HintHL<Bar>echo "void glGetInteger64i_v(GLenum target, GLuint index, GLint64 *data)"<Bar>echoh None<cr>
inorea glGetBufferParameteri64v glGetBufferParameteri64v<c-o>:echoh HintHL<Bar>echo "void glGetBufferParameteri64v(GLenum target, GLenum pname, GLint64 *params)"<Bar>echoh None<cr>
inorea glFramebufferTexture glFramebufferTexture<c-o>:echoh HintHL<Bar>echo "void glFramebufferTexture(GLenum target, GLenum attachment, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glVertexAttribDivisor glVertexAttribDivisor<c-o>:echoh HintHL<Bar>echo "void glVertexAttribDivisor(GLuint index, GLuint divisor)"<Bar>echoh None<cr>
inorea glMinSampleShading glMinSampleShading<c-o>:echoh HintHL<Bar>echo "void glMinSampleShading(GLclampf value)"<Bar>echoh None<cr>
inorea glBlendEquationi glBlendEquationi<c-o>:echoh HintHL<Bar>echo "void glBlendEquationi(GLuint buf, GLenum mode)"<Bar>echoh None<cr>
inorea glBlendEquationSeparatei glBlendEquationSeparatei<c-o>:echoh HintHL<Bar>echo "void glBlendEquationSeparatei(GLuint buf, GLenum modeRGB, GLenum modeAlpha)"<Bar>echoh None<cr>
inorea glBlendFunci glBlendFunci<c-o>:echoh HintHL<Bar>echo "void glBlendFunci(GLuint buf, GLenum src, GLenum dst)"<Bar>echoh None<cr>
inorea glBlendFuncSeparatei glBlendFuncSeparatei<c-o>:echoh HintHL<Bar>echo "void glBlendFuncSeparatei(GLuint buf, GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha)"<Bar>echoh None<cr>
inorea glActiveTextureARB glActiveTextureARB<c-o>:echoh HintHL<Bar>echo "void glActiveTextureARB(GLenum texture)"<Bar>echoh None<cr>
inorea glClientActiveTextureARB glClientActiveTextureARB<c-o>:echoh HintHL<Bar>echo "void glClientActiveTextureARB(GLenum texture)"<Bar>echoh None<cr>
inorea glMultiTexCoord1dARB glMultiTexCoord1dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1dARB(GLenum target, GLdouble s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1dvARB glMultiTexCoord1dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1fARB glMultiTexCoord1fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1fARB(GLenum target, GLfloat s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1fvARB glMultiTexCoord1fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1iARB glMultiTexCoord1iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1iARB(GLenum target, GLint s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1ivARB glMultiTexCoord1ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1sARB glMultiTexCoord1sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1sARB(GLenum target, GLshort s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1svARB glMultiTexCoord1svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2dARB glMultiTexCoord2dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2dARB(GLenum target, GLdouble s, GLdouble t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2dvARB glMultiTexCoord2dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2fARB glMultiTexCoord2fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2fARB(GLenum target, GLfloat s, GLfloat t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2fvARB glMultiTexCoord2fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2iARB glMultiTexCoord2iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2iARB(GLenum target, GLint s, GLint t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2ivARB glMultiTexCoord2ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2sARB glMultiTexCoord2sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2sARB(GLenum target, GLshort s, GLshort t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2svARB glMultiTexCoord2svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3dARB glMultiTexCoord3dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3dvARB glMultiTexCoord3dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3fARB glMultiTexCoord3fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3fvARB glMultiTexCoord3fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3iARB glMultiTexCoord3iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3iARB(GLenum target, GLint s, GLint t, GLint r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3ivARB glMultiTexCoord3ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3sARB glMultiTexCoord3sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3sARB(GLenum target, GLshort s, GLshort t, GLshort r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3svARB glMultiTexCoord3svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4dARB glMultiTexCoord4dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4dvARB glMultiTexCoord4dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4fARB glMultiTexCoord4fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4fvARB glMultiTexCoord4fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4iARB glMultiTexCoord4iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4iARB(GLenum target, GLint s, GLint t, GLint r, GLint q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4ivARB glMultiTexCoord4ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4sARB glMultiTexCoord4sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4sARB(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4svARB glMultiTexCoord4svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glLoadTransposeMatrixfARB glLoadTransposeMatrixfARB<c-o>:echoh HintHL<Bar>echo "void glLoadTransposeMatrixfARB(const GLfloat *m)"<Bar>echoh None<cr>
inorea glLoadTransposeMatrixdARB glLoadTransposeMatrixdARB<c-o>:echoh HintHL<Bar>echo "void glLoadTransposeMatrixdARB(const GLdouble *m)"<Bar>echoh None<cr>
inorea glMultTransposeMatrixfARB glMultTransposeMatrixfARB<c-o>:echoh HintHL<Bar>echo "void glMultTransposeMatrixfARB(const GLfloat *m)"<Bar>echoh None<cr>
inorea glMultTransposeMatrixdARB glMultTransposeMatrixdARB<c-o>:echoh HintHL<Bar>echo "void glMultTransposeMatrixdARB(const GLdouble *m)"<Bar>echoh None<cr>
inorea glSampleCoverageARB glSampleCoverageARB<c-o>:echoh HintHL<Bar>echo "void glSampleCoverageARB(GLclampf value, GLboolean invert)"<Bar>echoh None<cr>
inorea glCompressedTexImage3DARB glCompressedTexImage3DARB<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage3DARB(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexImage2DARB glCompressedTexImage2DARB<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage2DARB(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexImage1DARB glCompressedTexImage1DARB<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage1DARB(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage3DARB glCompressedTexSubImage3DARB<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage3DARB(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage2DARB glCompressedTexSubImage2DARB<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage2DARB(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage1DARB glCompressedTexSubImage1DARB<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage1DARB(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glGetCompressedTexImageARB glGetCompressedTexImageARB<c-o>:echoh HintHL<Bar>echo "void glGetCompressedTexImageARB(GLenum target, GLint level, GLvoid *img)"<Bar>echoh None<cr>
inorea glPointParameterfARB glPointParameterfARB<c-o>:echoh HintHL<Bar>echo "void glPointParameterfARB(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPointParameterfvARB glPointParameterfvARB<c-o>:echoh HintHL<Bar>echo "void glPointParameterfvARB(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glWeightbvARB glWeightbvARB<c-o>:echoh HintHL<Bar>echo "void glWeightbvARB(GLint size, const GLbyte *weights)"<Bar>echoh None<cr>
inorea glWeightsvARB glWeightsvARB<c-o>:echoh HintHL<Bar>echo "void glWeightsvARB(GLint size, const GLshort *weights)"<Bar>echoh None<cr>
inorea glWeightivARB glWeightivARB<c-o>:echoh HintHL<Bar>echo "void glWeightivARB(GLint size, const GLint *weights)"<Bar>echoh None<cr>
inorea glWeightfvARB glWeightfvARB<c-o>:echoh HintHL<Bar>echo "void glWeightfvARB(GLint size, const GLfloat *weights)"<Bar>echoh None<cr>
inorea glWeightdvARB glWeightdvARB<c-o>:echoh HintHL<Bar>echo "void glWeightdvARB(GLint size, const GLdouble *weights)"<Bar>echoh None<cr>
inorea glWeightubvARB glWeightubvARB<c-o>:echoh HintHL<Bar>echo "void glWeightubvARB(GLint size, const GLubyte *weights)"<Bar>echoh None<cr>
inorea glWeightusvARB glWeightusvARB<c-o>:echoh HintHL<Bar>echo "void glWeightusvARB(GLint size, const GLushort *weights)"<Bar>echoh None<cr>
inorea glWeightuivARB glWeightuivARB<c-o>:echoh HintHL<Bar>echo "void glWeightuivARB(GLint size, const GLuint *weights)"<Bar>echoh None<cr>
inorea glWeightPointerARB glWeightPointerARB<c-o>:echoh HintHL<Bar>echo "void glWeightPointerARB(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glVertexBlendARB glVertexBlendARB<c-o>:echoh HintHL<Bar>echo "void glVertexBlendARB(GLint count)"<Bar>echoh None<cr>
inorea glCurrentPaletteMatrixARB glCurrentPaletteMatrixARB<c-o>:echoh HintHL<Bar>echo "void glCurrentPaletteMatrixARB(GLint index)"<Bar>echoh None<cr>
inorea glMatrixIndexubvARB glMatrixIndexubvARB<c-o>:echoh HintHL<Bar>echo "void glMatrixIndexubvARB(GLint size, const GLubyte *indices)"<Bar>echoh None<cr>
inorea glMatrixIndexusvARB glMatrixIndexusvARB<c-o>:echoh HintHL<Bar>echo "void glMatrixIndexusvARB(GLint size, const GLushort *indices)"<Bar>echoh None<cr>
inorea glMatrixIndexuivARB glMatrixIndexuivARB<c-o>:echoh HintHL<Bar>echo "void glMatrixIndexuivARB(GLint size, const GLuint *indices)"<Bar>echoh None<cr>
inorea glMatrixIndexPointerARB glMatrixIndexPointerARB<c-o>:echoh HintHL<Bar>echo "void glMatrixIndexPointerARB(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glWindowPos2dARB glWindowPos2dARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2dARB(GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glWindowPos2dvARB glWindowPos2dvARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2dvARB(const GLdouble *v)"<Bar>echoh None<cr>
inorea glWindowPos2fARB glWindowPos2fARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2fARB(GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glWindowPos2fvARB glWindowPos2fvARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2fvARB(const GLfloat *v)"<Bar>echoh None<cr>
inorea glWindowPos2iARB glWindowPos2iARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2iARB(GLint x, GLint y)"<Bar>echoh None<cr>
inorea glWindowPos2ivARB glWindowPos2ivARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2ivARB(const GLint *v)"<Bar>echoh None<cr>
inorea glWindowPos2sARB glWindowPos2sARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2sARB(GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glWindowPos2svARB glWindowPos2svARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos2svARB(const GLshort *v)"<Bar>echoh None<cr>
inorea glWindowPos3dARB glWindowPos3dARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3dARB(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glWindowPos3dvARB glWindowPos3dvARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3dvARB(const GLdouble *v)"<Bar>echoh None<cr>
inorea glWindowPos3fARB glWindowPos3fARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3fARB(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glWindowPos3fvARB glWindowPos3fvARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3fvARB(const GLfloat *v)"<Bar>echoh None<cr>
inorea glWindowPos3iARB glWindowPos3iARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3iARB(GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glWindowPos3ivARB glWindowPos3ivARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3ivARB(const GLint *v)"<Bar>echoh None<cr>
inorea glWindowPos3sARB glWindowPos3sARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3sARB(GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glWindowPos3svARB glWindowPos3svARB<c-o>:echoh HintHL<Bar>echo "void glWindowPos3svARB(const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib1dARB glVertexAttrib1dARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1dARB(GLuint index, GLdouble x)"<Bar>echoh None<cr>
inorea glVertexAttrib1dvARB glVertexAttrib1dvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1dvARB(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib1fARB glVertexAttrib1fARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1fARB(GLuint index, GLfloat x)"<Bar>echoh None<cr>
inorea glVertexAttrib1fvARB glVertexAttrib1fvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1fvARB(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib1sARB glVertexAttrib1sARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1sARB(GLuint index, GLshort x)"<Bar>echoh None<cr>
inorea glVertexAttrib1svARB glVertexAttrib1svARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1svARB(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2dARB glVertexAttrib2dARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2dARB(GLuint index, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertexAttrib2dvARB glVertexAttrib2dvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2dvARB(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2fARB glVertexAttrib2fARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2fARB(GLuint index, GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glVertexAttrib2fvARB glVertexAttrib2fvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2fvARB(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2sARB glVertexAttrib2sARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2sARB(GLuint index, GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glVertexAttrib2svARB glVertexAttrib2svARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2svARB(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3dARB glVertexAttrib3dARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3dARB(GLuint index, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertexAttrib3dvARB glVertexAttrib3dvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3dvARB(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3fARB glVertexAttrib3fARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3fARB(GLuint index, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glVertexAttrib3fvARB glVertexAttrib3fvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3fvARB(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3sARB glVertexAttrib3sARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3sARB(GLuint index, GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glVertexAttrib3svARB glVertexAttrib3svARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3svARB(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4NbvARB glVertexAttrib4NbvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4NbvARB(GLuint index, const GLbyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4NivARB glVertexAttrib4NivARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4NivARB(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4NsvARB glVertexAttrib4NsvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4NsvARB(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4NubARB glVertexAttrib4NubARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4NubARB(GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w)"<Bar>echoh None<cr>
inorea glVertexAttrib4NubvARB glVertexAttrib4NubvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4NubvARB(GLuint index, const GLubyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4NuivARB glVertexAttrib4NuivARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4NuivARB(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4NusvARB glVertexAttrib4NusvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4NusvARB(GLuint index, const GLushort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4bvARB glVertexAttrib4bvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4bvARB(GLuint index, const GLbyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4dARB glVertexAttrib4dARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4dARB(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertexAttrib4dvARB glVertexAttrib4dvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4dvARB(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4fARB glVertexAttrib4fARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4fARB(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glVertexAttrib4fvARB glVertexAttrib4fvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4fvARB(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4ivARB glVertexAttrib4ivARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4ivARB(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4sARB glVertexAttrib4sARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4sARB(GLuint index, GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glVertexAttrib4svARB glVertexAttrib4svARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4svARB(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4ubvARB glVertexAttrib4ubvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4ubvARB(GLuint index, const GLubyte *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4uivARB glVertexAttrib4uivARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4uivARB(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4usvARB glVertexAttrib4usvARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4usvARB(GLuint index, const GLushort *v)"<Bar>echoh None<cr>
inorea glVertexAttribPointerARB glVertexAttribPointerARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttribPointerARB(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glEnableVertexAttribArrayARB glEnableVertexAttribArrayARB<c-o>:echoh HintHL<Bar>echo "void glEnableVertexAttribArrayARB(GLuint index)"<Bar>echoh None<cr>
inorea glDisableVertexAttribArrayARB glDisableVertexAttribArrayARB<c-o>:echoh HintHL<Bar>echo "void glDisableVertexAttribArrayARB(GLuint index)"<Bar>echoh None<cr>
inorea glProgramStringARB glProgramStringARB<c-o>:echoh HintHL<Bar>echo "void glProgramStringARB(GLenum target, GLenum format, GLsizei len, const GLvoid *string)"<Bar>echoh None<cr>
inorea glBindProgramARB glBindProgramARB<c-o>:echoh HintHL<Bar>echo "void glBindProgramARB(GLenum target, GLuint program)"<Bar>echoh None<cr>
inorea glDeleteProgramsARB glDeleteProgramsARB<c-o>:echoh HintHL<Bar>echo "void glDeleteProgramsARB(GLsizei n, const GLuint *programs)"<Bar>echoh None<cr>
inorea glGenProgramsARB glGenProgramsARB<c-o>:echoh HintHL<Bar>echo "void glGenProgramsARB(GLsizei n, GLuint *programs)"<Bar>echoh None<cr>
inorea glProgramEnvParameter4dARB glProgramEnvParameter4dARB<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameter4dARB(GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glProgramEnvParameter4dvARB glProgramEnvParameter4dvARB<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameter4dvARB(GLenum target, GLuint index, const GLdouble *params)"<Bar>echoh None<cr>
inorea glProgramEnvParameter4fARB glProgramEnvParameter4fARB<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameter4fARB(GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glProgramEnvParameter4fvARB glProgramEnvParameter4fvARB<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameter4fvARB(GLenum target, GLuint index, const GLfloat *params)"<Bar>echoh None<cr>
inorea glProgramLocalParameter4dARB glProgramLocalParameter4dARB<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameter4dARB(GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glProgramLocalParameter4dvARB glProgramLocalParameter4dvARB<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameter4dvARB(GLenum target, GLuint index, const GLdouble *params)"<Bar>echoh None<cr>
inorea glProgramLocalParameter4fARB glProgramLocalParameter4fARB<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameter4fARB(GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glProgramLocalParameter4fvARB glProgramLocalParameter4fvARB<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameter4fvARB(GLenum target, GLuint index, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetProgramEnvParameterdvARB glGetProgramEnvParameterdvARB<c-o>:echoh HintHL<Bar>echo "void glGetProgramEnvParameterdvARB(GLenum target, GLuint index, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetProgramEnvParameterfvARB glGetProgramEnvParameterfvARB<c-o>:echoh HintHL<Bar>echo "void glGetProgramEnvParameterfvARB(GLenum target, GLuint index, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetProgramLocalParameterdvARB glGetProgramLocalParameterdvARB<c-o>:echoh HintHL<Bar>echo "void glGetProgramLocalParameterdvARB(GLenum target, GLuint index, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetProgramLocalParameterfvARB glGetProgramLocalParameterfvARB<c-o>:echoh HintHL<Bar>echo "void glGetProgramLocalParameterfvARB(GLenum target, GLuint index, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetProgramivARB glGetProgramivARB<c-o>:echoh HintHL<Bar>echo "void glGetProgramivARB(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetProgramStringARB glGetProgramStringARB<c-o>:echoh HintHL<Bar>echo "void glGetProgramStringARB(GLenum target, GLenum pname, GLvoid *string)"<Bar>echoh None<cr>
inorea glGetVertexAttribdvARB glGetVertexAttribdvARB<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribdvARB(GLuint index, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribfvARB glGetVertexAttribfvARB<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribfvARB(GLuint index, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribivARB glGetVertexAttribivARB<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribivARB(GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribPointervARB glGetVertexAttribPointervARB<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribPointervARB(GLuint index, GLenum pname, GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glIsProgramARB glIsProgramARB<c-o>:echoh HintHL<Bar>echo "GLboolean glIsProgramARB(GLuint program)"<Bar>echoh None<cr>
inorea glBindBufferARB glBindBufferARB<c-o>:echoh HintHL<Bar>echo "void glBindBufferARB(GLenum target, GLuint buffer)"<Bar>echoh None<cr>
inorea glDeleteBuffersARB glDeleteBuffersARB<c-o>:echoh HintHL<Bar>echo "void glDeleteBuffersARB(GLsizei n, const GLuint *buffers)"<Bar>echoh None<cr>
inorea glGenBuffersARB glGenBuffersARB<c-o>:echoh HintHL<Bar>echo "void glGenBuffersARB(GLsizei n, GLuint *buffers)"<Bar>echoh None<cr>
inorea glIsBufferARB glIsBufferARB<c-o>:echoh HintHL<Bar>echo "GLboolean glIsBufferARB(GLuint buffer)"<Bar>echoh None<cr>
inorea glBufferDataARB glBufferDataARB<c-o>:echoh HintHL<Bar>echo "void glBufferDataARB(GLenum target, GLsizeiptrARB size, const GLvoid *data, GLenum usage)"<Bar>echoh None<cr>
inorea glBufferSubDataARB glBufferSubDataARB<c-o>:echoh HintHL<Bar>echo "void glBufferSubDataARB(GLenum target, GLintptrARB offset, GLsizeiptrARB size, const GLvoid *data)"<Bar>echoh None<cr>
inorea glGetBufferSubDataARB glGetBufferSubDataARB<c-o>:echoh HintHL<Bar>echo "void glGetBufferSubDataARB(GLenum target, GLintptrARB offset, GLsizeiptrARB size, GLvoid *data)"<Bar>echoh None<cr>
inorea glMapBufferARB glMapBufferARB<c-o>:echoh HintHL<Bar>echo "GLvoid *glMapBufferARB(GLenum target, GLenum access)"<Bar>echoh None<cr>
inorea glUnmapBufferARB glUnmapBufferARB<c-o>:echoh HintHL<Bar>echo "GLboolean glUnmapBufferARB(GLenum target)"<Bar>echoh None<cr>
inorea glGetBufferParameterivARB glGetBufferParameterivARB<c-o>:echoh HintHL<Bar>echo "void glGetBufferParameterivARB(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetBufferPointervARB glGetBufferPointervARB<c-o>:echoh HintHL<Bar>echo "void glGetBufferPointervARB(GLenum target, GLenum pname, GLvoid* *params)"<Bar>echoh None<cr>
inorea glGenQueriesARB glGenQueriesARB<c-o>:echoh HintHL<Bar>echo "void glGenQueriesARB(GLsizei n, GLuint *ids)"<Bar>echoh None<cr>
inorea glDeleteQueriesARB glDeleteQueriesARB<c-o>:echoh HintHL<Bar>echo "void glDeleteQueriesARB(GLsizei n, const GLuint *ids)"<Bar>echoh None<cr>
inorea glIsQueryARB glIsQueryARB<c-o>:echoh HintHL<Bar>echo "GLboolean glIsQueryARB(GLuint id)"<Bar>echoh None<cr>
inorea glBeginQueryARB glBeginQueryARB<c-o>:echoh HintHL<Bar>echo "void glBeginQueryARB(GLenum target, GLuint id)"<Bar>echoh None<cr>
inorea glEndQueryARB glEndQueryARB<c-o>:echoh HintHL<Bar>echo "void glEndQueryARB(GLenum target)"<Bar>echoh None<cr>
inorea glGetQueryivARB glGetQueryivARB<c-o>:echoh HintHL<Bar>echo "void glGetQueryivARB(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetQueryObjectivARB glGetQueryObjectivARB<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjectivARB(GLuint id, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetQueryObjectuivARB glGetQueryObjectuivARB<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjectuivARB(GLuint id, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glDeleteObjectARB glDeleteObjectARB<c-o>:echoh HintHL<Bar>echo "void glDeleteObjectARB(GLhandleARB obj)"<Bar>echoh None<cr>
inorea glGetHandleARB glGetHandleARB<c-o>:echoh HintHL<Bar>echo "GLhandleARB glGetHandleARB(GLenum pname)"<Bar>echoh None<cr>
inorea glDetachObjectARB glDetachObjectARB<c-o>:echoh HintHL<Bar>echo "void glDetachObjectARB(GLhandleARB containerObj, GLhandleARB attachedObj)"<Bar>echoh None<cr>
inorea glCreateShaderObjectARB glCreateShaderObjectARB<c-o>:echoh HintHL<Bar>echo "GLhandleARB glCreateShaderObjectARB(GLenum shaderType)"<Bar>echoh None<cr>
inorea glShaderSourceARB glShaderSourceARB<c-o>:echoh HintHL<Bar>echo "void glShaderSourceARB(GLhandleARB shaderObj, GLsizei count, const GLcharARB* *string, const GLint *length)"<Bar>echoh None<cr>
inorea glCompileShaderARB glCompileShaderARB<c-o>:echoh HintHL<Bar>echo "void glCompileShaderARB(GLhandleARB shaderObj)"<Bar>echoh None<cr>
inorea glCreateProgramObjectARB glCreateProgramObjectARB<c-o>:echoh HintHL<Bar>echo "GLhandleARB glCreateProgramObjectARB(void)"<Bar>echoh None<cr>
inorea glAttachObjectARB glAttachObjectARB<c-o>:echoh HintHL<Bar>echo "void glAttachObjectARB(GLhandleARB containerObj, GLhandleARB obj)"<Bar>echoh None<cr>
inorea glLinkProgramARB glLinkProgramARB<c-o>:echoh HintHL<Bar>echo "void glLinkProgramARB(GLhandleARB programObj)"<Bar>echoh None<cr>
inorea glUseProgramObjectARB glUseProgramObjectARB<c-o>:echoh HintHL<Bar>echo "void glUseProgramObjectARB(GLhandleARB programObj)"<Bar>echoh None<cr>
inorea glValidateProgramARB glValidateProgramARB<c-o>:echoh HintHL<Bar>echo "void glValidateProgramARB(GLhandleARB programObj)"<Bar>echoh None<cr>
inorea glUniform1fARB glUniform1fARB<c-o>:echoh HintHL<Bar>echo "void glUniform1fARB(GLint location, GLfloat v0)"<Bar>echoh None<cr>
inorea glUniform2fARB glUniform2fARB<c-o>:echoh HintHL<Bar>echo "void glUniform2fARB(GLint location, GLfloat v0, GLfloat v1)"<Bar>echoh None<cr>
inorea glUniform3fARB glUniform3fARB<c-o>:echoh HintHL<Bar>echo "void glUniform3fARB(GLint location, GLfloat v0, GLfloat v1, GLfloat v2)"<Bar>echoh None<cr>
inorea glUniform4fARB glUniform4fARB<c-o>:echoh HintHL<Bar>echo "void glUniform4fARB(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)"<Bar>echoh None<cr>
inorea glUniform1iARB glUniform1iARB<c-o>:echoh HintHL<Bar>echo "void glUniform1iARB(GLint location, GLint v0)"<Bar>echoh None<cr>
inorea glUniform2iARB glUniform2iARB<c-o>:echoh HintHL<Bar>echo "void glUniform2iARB(GLint location, GLint v0, GLint v1)"<Bar>echoh None<cr>
inorea glUniform3iARB glUniform3iARB<c-o>:echoh HintHL<Bar>echo "void glUniform3iARB(GLint location, GLint v0, GLint v1, GLint v2)"<Bar>echoh None<cr>
inorea glUniform4iARB glUniform4iARB<c-o>:echoh HintHL<Bar>echo "void glUniform4iARB(GLint location, GLint v0, GLint v1, GLint v2, GLint v3)"<Bar>echoh None<cr>
inorea glUniform1fvARB glUniform1fvARB<c-o>:echoh HintHL<Bar>echo "void glUniform1fvARB(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform2fvARB glUniform2fvARB<c-o>:echoh HintHL<Bar>echo "void glUniform2fvARB(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform3fvARB glUniform3fvARB<c-o>:echoh HintHL<Bar>echo "void glUniform3fvARB(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform4fvARB glUniform4fvARB<c-o>:echoh HintHL<Bar>echo "void glUniform4fvARB(GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniform1ivARB glUniform1ivARB<c-o>:echoh HintHL<Bar>echo "void glUniform1ivARB(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniform2ivARB glUniform2ivARB<c-o>:echoh HintHL<Bar>echo "void glUniform2ivARB(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniform3ivARB glUniform3ivARB<c-o>:echoh HintHL<Bar>echo "void glUniform3ivARB(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniform4ivARB glUniform4ivARB<c-o>:echoh HintHL<Bar>echo "void glUniform4ivARB(GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glUniformMatrix2fvARB glUniformMatrix2fvARB<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix2fvARB(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix3fvARB glUniformMatrix3fvARB<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix3fvARB(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glUniformMatrix4fvARB glUniformMatrix4fvARB<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix4fvARB(GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glGetObjectParameterfvARB glGetObjectParameterfvARB<c-o>:echoh HintHL<Bar>echo "void glGetObjectParameterfvARB(GLhandleARB obj, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetObjectParameterivARB glGetObjectParameterivARB<c-o>:echoh HintHL<Bar>echo "void glGetObjectParameterivARB(GLhandleARB obj, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetInfoLogARB glGetInfoLogARB<c-o>:echoh HintHL<Bar>echo "void glGetInfoLogARB(GLhandleARB obj, GLsizei maxLength, GLsizei *length, GLcharARB *infoLog)"<Bar>echoh None<cr>
inorea glGetAttachedObjectsARB glGetAttachedObjectsARB<c-o>:echoh HintHL<Bar>echo "void glGetAttachedObjectsARB(GLhandleARB containerObj, GLsizei maxCount, GLsizei *count, GLhandleARB *obj)"<Bar>echoh None<cr>
inorea glGetUniformLocationARB glGetUniformLocationARB<c-o>:echoh HintHL<Bar>echo "GLint glGetUniformLocationARB(GLhandleARB programObj, const GLcharARB *name)"<Bar>echoh None<cr>
inorea glGetActiveUniformARB glGetActiveUniformARB<c-o>:echoh HintHL<Bar>echo "void glGetActiveUniformARB(GLhandleARB programObj, GLuint index, GLsizei maxLength, GLsizei *length, GLint *size, GLenum *type, GLcharARB *name)"<Bar>echoh None<cr>
inorea glGetUniformfvARB glGetUniformfvARB<c-o>:echoh HintHL<Bar>echo "void glGetUniformfvARB(GLhandleARB programObj, GLint location, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetUniformivARB glGetUniformivARB<c-o>:echoh HintHL<Bar>echo "void glGetUniformivARB(GLhandleARB programObj, GLint location, GLint *params)"<Bar>echoh None<cr>
inorea glGetShaderSourceARB glGetShaderSourceARB<c-o>:echoh HintHL<Bar>echo "void glGetShaderSourceARB(GLhandleARB obj, GLsizei maxLength, GLsizei *length, GLcharARB *source)"<Bar>echoh None<cr>
inorea glBindAttribLocationARB glBindAttribLocationARB<c-o>:echoh HintHL<Bar>echo "void glBindAttribLocationARB(GLhandleARB programObj, GLuint index, const GLcharARB *name)"<Bar>echoh None<cr>
inorea glGetActiveAttribARB glGetActiveAttribARB<c-o>:echoh HintHL<Bar>echo "void glGetActiveAttribARB(GLhandleARB programObj, GLuint index, GLsizei maxLength, GLsizei *length, GLint *size, GLenum *type, GLcharARB *name)"<Bar>echoh None<cr>
inorea glGetAttribLocationARB glGetAttribLocationARB<c-o>:echoh HintHL<Bar>echo "GLint glGetAttribLocationARB(GLhandleARB programObj, const GLcharARB *name)"<Bar>echoh None<cr>
inorea glDrawBuffersARB glDrawBuffersARB<c-o>:echoh HintHL<Bar>echo "void glDrawBuffersARB(GLsizei n, const GLenum *bufs)"<Bar>echoh None<cr>
inorea glClampColorARB glClampColorARB<c-o>:echoh HintHL<Bar>echo "void glClampColorARB(GLenum target, GLenum clamp)"<Bar>echoh None<cr>
inorea glDrawArraysInstancedARB glDrawArraysInstancedARB<c-o>:echoh HintHL<Bar>echo "void glDrawArraysInstancedARB(GLenum mode, GLint first, GLsizei count, GLsizei primcount)"<Bar>echoh None<cr>
inorea glDrawElementsInstancedARB glDrawElementsInstancedARB<c-o>:echoh HintHL<Bar>echo "void glDrawElementsInstancedARB(GLenum mode, GLsizei count, GLenum type, const GLvoid *indices, GLsizei primcount)"<Bar>echoh None<cr>
inorea glIsRenderbuffer glIsRenderbuffer<c-o>:echoh HintHL<Bar>echo "GLboolean glIsRenderbuffer(GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glBindRenderbuffer glBindRenderbuffer<c-o>:echoh HintHL<Bar>echo "void glBindRenderbuffer(GLenum target, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glDeleteRenderbuffers glDeleteRenderbuffers<c-o>:echoh HintHL<Bar>echo "void glDeleteRenderbuffers(GLsizei n, const GLuint *renderbuffers)"<Bar>echoh None<cr>
inorea glGenRenderbuffers glGenRenderbuffers<c-o>:echoh HintHL<Bar>echo "void glGenRenderbuffers(GLsizei n, GLuint *renderbuffers)"<Bar>echoh None<cr>
inorea glRenderbufferStorage glRenderbufferStorage<c-o>:echoh HintHL<Bar>echo "void glRenderbufferStorage(GLenum target, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetRenderbufferParameteriv glGetRenderbufferParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetRenderbufferParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glIsFramebuffer glIsFramebuffer<c-o>:echoh HintHL<Bar>echo "GLboolean glIsFramebuffer(GLuint framebuffer)"<Bar>echoh None<cr>
inorea glBindFramebuffer glBindFramebuffer<c-o>:echoh HintHL<Bar>echo "void glBindFramebuffer(GLenum target, GLuint framebuffer)"<Bar>echoh None<cr>
inorea glDeleteFramebuffers glDeleteFramebuffers<c-o>:echoh HintHL<Bar>echo "void glDeleteFramebuffers(GLsizei n, const GLuint *framebuffers)"<Bar>echoh None<cr>
inorea glGenFramebuffers glGenFramebuffers<c-o>:echoh HintHL<Bar>echo "void glGenFramebuffers(GLsizei n, GLuint *framebuffers)"<Bar>echoh None<cr>
inorea glCheckFramebufferStatus glCheckFramebufferStatus<c-o>:echoh HintHL<Bar>echo "GLenum glCheckFramebufferStatus(GLenum target)"<Bar>echoh None<cr>
inorea glFramebufferTexture1D glFramebufferTexture1D<c-o>:echoh HintHL<Bar>echo "void glFramebufferTexture1D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glFramebufferTexture2D glFramebufferTexture2D<c-o>:echoh HintHL<Bar>echo "void glFramebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glFramebufferTexture3D glFramebufferTexture3D<c-o>:echoh HintHL<Bar>echo "void glFramebufferTexture3D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level, GLint zoffset)"<Bar>echoh None<cr>
inorea glFramebufferRenderbuffer glFramebufferRenderbuffer<c-o>:echoh HintHL<Bar>echo "void glFramebufferRenderbuffer(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glGetFramebufferAttachmentParameteriv glGetFramebufferAttachmentParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetFramebufferAttachmentParameteriv(GLenum target, GLenum attachment, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGenerateMipmap glGenerateMipmap<c-o>:echoh HintHL<Bar>echo "void glGenerateMipmap(GLenum target)"<Bar>echoh None<cr>
inorea glBlitFramebuffer glBlitFramebuffer<c-o>:echoh HintHL<Bar>echo "void glBlitFramebuffer(GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0, GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter)"<Bar>echoh None<cr>
inorea glRenderbufferStorageMultisample glRenderbufferStorageMultisample<c-o>:echoh HintHL<Bar>echo "void glRenderbufferStorageMultisample(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glFramebufferTextureLayer glFramebufferTextureLayer<c-o>:echoh HintHL<Bar>echo "void glFramebufferTextureLayer(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer)"<Bar>echoh None<cr>
inorea glProgramParameteriARB glProgramParameteriARB<c-o>:echoh HintHL<Bar>echo "void glProgramParameteriARB(GLuint program, GLenum pname, GLint value)"<Bar>echoh None<cr>
inorea glFramebufferTextureARB glFramebufferTextureARB<c-o>:echoh HintHL<Bar>echo "void glFramebufferTextureARB(GLenum target, GLenum attachment, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glFramebufferTextureLayerARB glFramebufferTextureLayerARB<c-o>:echoh HintHL<Bar>echo "void glFramebufferTextureLayerARB(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer)"<Bar>echoh None<cr>
inorea glFramebufferTextureFaceARB glFramebufferTextureFaceARB<c-o>:echoh HintHL<Bar>echo "void glFramebufferTextureFaceARB(GLenum target, GLenum attachment, GLuint texture, GLint level, GLenum face)"<Bar>echoh None<cr>
inorea glVertexAttribDivisorARB glVertexAttribDivisorARB<c-o>:echoh HintHL<Bar>echo "void glVertexAttribDivisorARB(GLuint index, GLuint divisor)"<Bar>echoh None<cr>
inorea glMapBufferRange glMapBufferRange<c-o>:echoh HintHL<Bar>echo "GLvoid *glMapBufferRange(GLenum target, GLintptr offset, GLsizeiptr length, GLbitfield access)"<Bar>echoh None<cr>
inorea glFlushMappedBufferRange glFlushMappedBufferRange<c-o>:echoh HintHL<Bar>echo "void glFlushMappedBufferRange(GLenum target, GLintptr offset, GLsizeiptr length)"<Bar>echoh None<cr>
inorea glTexBufferARB glTexBufferARB<c-o>:echoh HintHL<Bar>echo "void glTexBufferARB(GLenum target, GLenum internalformat, GLuint buffer)"<Bar>echoh None<cr>
inorea glBindVertexArray glBindVertexArray<c-o>:echoh HintHL<Bar>echo "void glBindVertexArray(GLuint array)"<Bar>echoh None<cr>
inorea glDeleteVertexArrays glDeleteVertexArrays<c-o>:echoh HintHL<Bar>echo "void glDeleteVertexArrays(GLsizei n, const GLuint *arrays)"<Bar>echoh None<cr>
inorea glGenVertexArrays glGenVertexArrays<c-o>:echoh HintHL<Bar>echo "void glGenVertexArrays(GLsizei n, GLuint *arrays)"<Bar>echoh None<cr>
inorea glIsVertexArray glIsVertexArray<c-o>:echoh HintHL<Bar>echo "GLboolean glIsVertexArray(GLuint array)"<Bar>echoh None<cr>
inorea glGetUniformIndices glGetUniformIndices<c-o>:echoh HintHL<Bar>echo "void glGetUniformIndices(GLuint program, GLsizei uniformCount, const GLchar* *uniformNames, GLuint *uniformIndices)"<Bar>echoh None<cr>
inorea glGetActiveUniformsiv glGetActiveUniformsiv<c-o>:echoh HintHL<Bar>echo "void glGetActiveUniformsiv(GLuint program, GLsizei uniformCount, const GLuint *uniformIndices, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetActiveUniformName glGetActiveUniformName<c-o>:echoh HintHL<Bar>echo "void glGetActiveUniformName(GLuint program, GLuint uniformIndex, GLsizei bufSize, GLsizei *length, GLchar *uniformName)"<Bar>echoh None<cr>
inorea glGetUniformBlockIndex glGetUniformBlockIndex<c-o>:echoh HintHL<Bar>echo "GLuint glGetUniformBlockIndex(GLuint program, const GLchar *uniformBlockName)"<Bar>echoh None<cr>
inorea glGetActiveUniformBlockiv glGetActiveUniformBlockiv<c-o>:echoh HintHL<Bar>echo "void glGetActiveUniformBlockiv(GLuint program, GLuint uniformBlockIndex, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetActiveUniformBlockName glGetActiveUniformBlockName<c-o>:echoh HintHL<Bar>echo "void glGetActiveUniformBlockName(GLuint program, GLuint uniformBlockIndex, GLsizei bufSize, GLsizei *length, GLchar *uniformBlockName)"<Bar>echoh None<cr>
inorea glUniformBlockBinding glUniformBlockBinding<c-o>:echoh HintHL<Bar>echo "void glUniformBlockBinding(GLuint program, GLuint uniformBlockIndex, GLuint uniformBlockBinding)"<Bar>echoh None<cr>
inorea glCopyBufferSubData glCopyBufferSubData<c-o>:echoh HintHL<Bar>echo "void glCopyBufferSubData(GLenum readTarget, GLenum writeTarget, GLintptr readOffset, GLintptr writeOffset, GLsizeiptr size)"<Bar>echoh None<cr>
inorea glDrawElementsBaseVertex glDrawElementsBaseVertex<c-o>:echoh HintHL<Bar>echo "void glDrawElementsBaseVertex(GLenum mode, GLsizei count, GLenum type, const GLvoid *indices, GLint basevertex)"<Bar>echoh None<cr>
inorea glDrawRangeElementsBaseVertex glDrawRangeElementsBaseVertex<c-o>:echoh HintHL<Bar>echo "void glDrawRangeElementsBaseVertex(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices, GLint basevertex)"<Bar>echoh None<cr>
inorea glDrawElementsInstancedBaseVertex glDrawElementsInstancedBaseVertex<c-o>:echoh HintHL<Bar>echo "void glDrawElementsInstancedBaseVertex(GLenum mode, GLsizei count, GLenum type, const GLvoid *indices, GLsizei primcount, GLint basevertex)"<Bar>echoh None<cr>
inorea glMultiDrawElementsBaseVertex glMultiDrawElementsBaseVertex<c-o>:echoh HintHL<Bar>echo "void glMultiDrawElementsBaseVertex(GLenum mode, const GLsizei *count, GLenum type, const GLvoid* *indices, GLsizei primcount, const GLint *basevertex)"<Bar>echoh None<cr>
inorea glProvokingVertex glProvokingVertex<c-o>:echoh HintHL<Bar>echo "void glProvokingVertex(GLenum mode)"<Bar>echoh None<cr>
inorea glFenceSync glFenceSync<c-o>:echoh HintHL<Bar>echo "GLsync glFenceSync(GLenum condition, GLbitfield flags)"<Bar>echoh None<cr>
inorea glIsSync glIsSync<c-o>:echoh HintHL<Bar>echo "GLboolean glIsSync(GLsync sync)"<Bar>echoh None<cr>
inorea glDeleteSync glDeleteSync<c-o>:echoh HintHL<Bar>echo "void glDeleteSync(GLsync sync)"<Bar>echoh None<cr>
inorea glClientWaitSync glClientWaitSync<c-o>:echoh HintHL<Bar>echo "GLenum glClientWaitSync(GLsync sync, GLbitfield flags, GLuint64 timeout)"<Bar>echoh None<cr>
inorea glWaitSync glWaitSync<c-o>:echoh HintHL<Bar>echo "void glWaitSync(GLsync sync, GLbitfield flags, GLuint64 timeout)"<Bar>echoh None<cr>
inorea glGetInteger64v glGetInteger64v<c-o>:echoh HintHL<Bar>echo "void glGetInteger64v(GLenum pname, GLint64 *params)"<Bar>echoh None<cr>
inorea glGetSynciv glGetSynciv<c-o>:echoh HintHL<Bar>echo "void glGetSynciv(GLsync sync, GLenum pname, GLsizei bufSize, GLsizei *length, GLint *values)"<Bar>echoh None<cr>
inorea glTexImage2DMultisample glTexImage2DMultisample<c-o>:echoh HintHL<Bar>echo "void glTexImage2DMultisample(GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLboolean fixedsamplelocations)"<Bar>echoh None<cr>
inorea glTexImage3DMultisample glTexImage3DMultisample<c-o>:echoh HintHL<Bar>echo "void glTexImage3DMultisample(GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean fixedsamplelocations)"<Bar>echoh None<cr>
inorea glGetMultisamplefv glGetMultisamplefv<c-o>:echoh HintHL<Bar>echo "void glGetMultisamplefv(GLenum pname, GLuint index, GLfloat *val)"<Bar>echoh None<cr>
inorea glSampleMaski glSampleMaski<c-o>:echoh HintHL<Bar>echo "void glSampleMaski(GLuint index, GLbitfield mask)"<Bar>echoh None<cr>
inorea glBlendEquationiARB glBlendEquationiARB<c-o>:echoh HintHL<Bar>echo "void glBlendEquationiARB(GLuint buf, GLenum mode)"<Bar>echoh None<cr>
inorea glBlendEquationSeparateiARB glBlendEquationSeparateiARB<c-o>:echoh HintHL<Bar>echo "void glBlendEquationSeparateiARB(GLuint buf, GLenum modeRGB, GLenum modeAlpha)"<Bar>echoh None<cr>
inorea glBlendFunciARB glBlendFunciARB<c-o>:echoh HintHL<Bar>echo "void glBlendFunciARB(GLuint buf, GLenum src, GLenum dst)"<Bar>echoh None<cr>
inorea glBlendFuncSeparateiARB glBlendFuncSeparateiARB<c-o>:echoh HintHL<Bar>echo "void glBlendFuncSeparateiARB(GLuint buf, GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha)"<Bar>echoh None<cr>
inorea glMinSampleShadingARB glMinSampleShadingARB<c-o>:echoh HintHL<Bar>echo "void glMinSampleShadingARB(GLclampf value)"<Bar>echoh None<cr>
inorea glNamedStringARB glNamedStringARB<c-o>:echoh HintHL<Bar>echo "void glNamedStringARB(GLenum type, GLint namelen, const GLchar *name, GLint stringlen, const GLchar *string)"<Bar>echoh None<cr>
inorea glDeleteNamedStringARB glDeleteNamedStringARB<c-o>:echoh HintHL<Bar>echo "void glDeleteNamedStringARB(GLint namelen, const GLchar *name)"<Bar>echoh None<cr>
inorea glCompileShaderIncludeARB glCompileShaderIncludeARB<c-o>:echoh HintHL<Bar>echo "void glCompileShaderIncludeARB(GLuint shader, GLsizei count, const GLchar* *path, const GLint *length)"<Bar>echoh None<cr>
inorea glIsNamedStringARB glIsNamedStringARB<c-o>:echoh HintHL<Bar>echo "GLboolean glIsNamedStringARB(GLint namelen, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetNamedStringARB glGetNamedStringARB<c-o>:echoh HintHL<Bar>echo "void glGetNamedStringARB(GLint namelen, const GLchar *name, GLsizei bufSize, GLint *stringlen, GLchar *string)"<Bar>echoh None<cr>
inorea glGetNamedStringivARB glGetNamedStringivARB<c-o>:echoh HintHL<Bar>echo "void glGetNamedStringivARB(GLint namelen, const GLchar *name, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glBindFragDataLocationIndexed glBindFragDataLocationIndexed<c-o>:echoh HintHL<Bar>echo "void glBindFragDataLocationIndexed(GLuint program, GLuint colorNumber, GLuint index, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetFragDataIndex glGetFragDataIndex<c-o>:echoh HintHL<Bar>echo "GLint glGetFragDataIndex(GLuint program, const GLchar *name)"<Bar>echoh None<cr>
inorea glGenSamplers glGenSamplers<c-o>:echoh HintHL<Bar>echo "void glGenSamplers(GLsizei count, GLuint *samplers)"<Bar>echoh None<cr>
inorea glDeleteSamplers glDeleteSamplers<c-o>:echoh HintHL<Bar>echo "void glDeleteSamplers(GLsizei count, const GLuint *samplers)"<Bar>echoh None<cr>
inorea glIsSampler glIsSampler<c-o>:echoh HintHL<Bar>echo "GLboolean glIsSampler(GLuint sampler)"<Bar>echoh None<cr>
inorea glBindSampler glBindSampler<c-o>:echoh HintHL<Bar>echo "void glBindSampler(GLuint unit, GLuint sampler)"<Bar>echoh None<cr>
inorea glSamplerParameteri glSamplerParameteri<c-o>:echoh HintHL<Bar>echo "void glSamplerParameteri(GLuint sampler, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glSamplerParameteriv glSamplerParameteriv<c-o>:echoh HintHL<Bar>echo "void glSamplerParameteriv(GLuint sampler, GLenum pname, const GLint *param)"<Bar>echoh None<cr>
inorea glSamplerParameterf glSamplerParameterf<c-o>:echoh HintHL<Bar>echo "void glSamplerParameterf(GLuint sampler, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glSamplerParameterfv glSamplerParameterfv<c-o>:echoh HintHL<Bar>echo "void glSamplerParameterfv(GLuint sampler, GLenum pname, const GLfloat *param)"<Bar>echoh None<cr>
inorea glSamplerParameterIiv glSamplerParameterIiv<c-o>:echoh HintHL<Bar>echo "void glSamplerParameterIiv(GLuint sampler, GLenum pname, const GLint *param)"<Bar>echoh None<cr>
inorea glSamplerParameterIuiv glSamplerParameterIuiv<c-o>:echoh HintHL<Bar>echo "void glSamplerParameterIuiv(GLuint sampler, GLenum pname, const GLuint *param)"<Bar>echoh None<cr>
inorea glGetSamplerParameteriv glGetSamplerParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetSamplerParameteriv(GLuint sampler, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetSamplerParameterIiv glGetSamplerParameterIiv<c-o>:echoh HintHL<Bar>echo "void glGetSamplerParameterIiv(GLuint sampler, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetSamplerParameterfv glGetSamplerParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetSamplerParameterfv(GLuint sampler, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetSamplerParameterIuiv glGetSamplerParameterIuiv<c-o>:echoh HintHL<Bar>echo "void glGetSamplerParameterIuiv(GLuint sampler, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glQueryCounter glQueryCounter<c-o>:echoh HintHL<Bar>echo "void glQueryCounter(GLuint id, GLenum target)"<Bar>echoh None<cr>
inorea glGetQueryObjecti64v glGetQueryObjecti64v<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjecti64v(GLuint id, GLenum pname, GLint64 *params)"<Bar>echoh None<cr>
inorea glGetQueryObjectui64v glGetQueryObjectui64v<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjectui64v(GLuint id, GLenum pname, GLuint64 *params)"<Bar>echoh None<cr>
inorea glVertexP2ui glVertexP2ui<c-o>:echoh HintHL<Bar>echo "void glVertexP2ui(GLenum type, GLuint value)"<Bar>echoh None<cr>
inorea glVertexP2uiv glVertexP2uiv<c-o>:echoh HintHL<Bar>echo "void glVertexP2uiv(GLenum type, const GLuint *value)"<Bar>echoh None<cr>
inorea glVertexP3ui glVertexP3ui<c-o>:echoh HintHL<Bar>echo "void glVertexP3ui(GLenum type, GLuint value)"<Bar>echoh None<cr>
inorea glVertexP3uiv glVertexP3uiv<c-o>:echoh HintHL<Bar>echo "void glVertexP3uiv(GLenum type, const GLuint *value)"<Bar>echoh None<cr>
inorea glVertexP4ui glVertexP4ui<c-o>:echoh HintHL<Bar>echo "void glVertexP4ui(GLenum type, GLuint value)"<Bar>echoh None<cr>
inorea glVertexP4uiv glVertexP4uiv<c-o>:echoh HintHL<Bar>echo "void glVertexP4uiv(GLenum type, const GLuint *value)"<Bar>echoh None<cr>
inorea glTexCoordP1ui glTexCoordP1ui<c-o>:echoh HintHL<Bar>echo "void glTexCoordP1ui(GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glTexCoordP1uiv glTexCoordP1uiv<c-o>:echoh HintHL<Bar>echo "void glTexCoordP1uiv(GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glTexCoordP2ui glTexCoordP2ui<c-o>:echoh HintHL<Bar>echo "void glTexCoordP2ui(GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glTexCoordP2uiv glTexCoordP2uiv<c-o>:echoh HintHL<Bar>echo "void glTexCoordP2uiv(GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glTexCoordP3ui glTexCoordP3ui<c-o>:echoh HintHL<Bar>echo "void glTexCoordP3ui(GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glTexCoordP3uiv glTexCoordP3uiv<c-o>:echoh HintHL<Bar>echo "void glTexCoordP3uiv(GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glTexCoordP4ui glTexCoordP4ui<c-o>:echoh HintHL<Bar>echo "void glTexCoordP4ui(GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glTexCoordP4uiv glTexCoordP4uiv<c-o>:echoh HintHL<Bar>echo "void glTexCoordP4uiv(GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP1ui glMultiTexCoordP1ui<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP1ui(GLenum texture, GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP1uiv glMultiTexCoordP1uiv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP1uiv(GLenum texture, GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP2ui glMultiTexCoordP2ui<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP2ui(GLenum texture, GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP2uiv glMultiTexCoordP2uiv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP2uiv(GLenum texture, GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP3ui glMultiTexCoordP3ui<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP3ui(GLenum texture, GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP3uiv glMultiTexCoordP3uiv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP3uiv(GLenum texture, GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP4ui glMultiTexCoordP4ui<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP4ui(GLenum texture, GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glMultiTexCoordP4uiv glMultiTexCoordP4uiv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordP4uiv(GLenum texture, GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glNormalP3ui glNormalP3ui<c-o>:echoh HintHL<Bar>echo "void glNormalP3ui(GLenum type, GLuint coords)"<Bar>echoh None<cr>
inorea glNormalP3uiv glNormalP3uiv<c-o>:echoh HintHL<Bar>echo "void glNormalP3uiv(GLenum type, const GLuint *coords)"<Bar>echoh None<cr>
inorea glColorP3ui glColorP3ui<c-o>:echoh HintHL<Bar>echo "void glColorP3ui(GLenum type, GLuint color)"<Bar>echoh None<cr>
inorea glColorP3uiv glColorP3uiv<c-o>:echoh HintHL<Bar>echo "void glColorP3uiv(GLenum type, const GLuint *color)"<Bar>echoh None<cr>
inorea glColorP4ui glColorP4ui<c-o>:echoh HintHL<Bar>echo "void glColorP4ui(GLenum type, GLuint color)"<Bar>echoh None<cr>
inorea glColorP4uiv glColorP4uiv<c-o>:echoh HintHL<Bar>echo "void glColorP4uiv(GLenum type, const GLuint *color)"<Bar>echoh None<cr>
inorea glSecondaryColorP3ui glSecondaryColorP3ui<c-o>:echoh HintHL<Bar>echo "void glSecondaryColorP3ui(GLenum type, GLuint color)"<Bar>echoh None<cr>
inorea glSecondaryColorP3uiv glSecondaryColorP3uiv<c-o>:echoh HintHL<Bar>echo "void glSecondaryColorP3uiv(GLenum type, const GLuint *color)"<Bar>echoh None<cr>
inorea glVertexAttribP1ui glVertexAttribP1ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP1ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)"<Bar>echoh None<cr>
inorea glVertexAttribP1uiv glVertexAttribP1uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP1uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)"<Bar>echoh None<cr>
inorea glVertexAttribP2ui glVertexAttribP2ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP2ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)"<Bar>echoh None<cr>
inorea glVertexAttribP2uiv glVertexAttribP2uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP2uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)"<Bar>echoh None<cr>
inorea glVertexAttribP3ui glVertexAttribP3ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP3ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)"<Bar>echoh None<cr>
inorea glVertexAttribP3uiv glVertexAttribP3uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP3uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)"<Bar>echoh None<cr>
inorea glVertexAttribP4ui glVertexAttribP4ui<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP4ui(GLuint index, GLenum type, GLboolean normalized, GLuint value)"<Bar>echoh None<cr>
inorea glVertexAttribP4uiv glVertexAttribP4uiv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribP4uiv(GLuint index, GLenum type, GLboolean normalized, const GLuint *value)"<Bar>echoh None<cr>
inorea glDrawArraysIndirect glDrawArraysIndirect<c-o>:echoh HintHL<Bar>echo "void glDrawArraysIndirect(GLenum mode, const GLvoid *indirect)"<Bar>echoh None<cr>
inorea glDrawElementsIndirect glDrawElementsIndirect<c-o>:echoh HintHL<Bar>echo "void glDrawElementsIndirect(GLenum mode, GLenum type, const GLvoid *indirect)"<Bar>echoh None<cr>
inorea glUniform1d glUniform1d<c-o>:echoh HintHL<Bar>echo "void glUniform1d(GLint location, GLdouble x)"<Bar>echoh None<cr>
inorea glUniform2d glUniform2d<c-o>:echoh HintHL<Bar>echo "void glUniform2d(GLint location, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glUniform3d glUniform3d<c-o>:echoh HintHL<Bar>echo "void glUniform3d(GLint location, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glUniform4d glUniform4d<c-o>:echoh HintHL<Bar>echo "void glUniform4d(GLint location, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glUniform1dv glUniform1dv<c-o>:echoh HintHL<Bar>echo "void glUniform1dv(GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniform2dv glUniform2dv<c-o>:echoh HintHL<Bar>echo "void glUniform2dv(GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniform3dv glUniform3dv<c-o>:echoh HintHL<Bar>echo "void glUniform3dv(GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniform4dv glUniform4dv<c-o>:echoh HintHL<Bar>echo "void glUniform4dv(GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix2dv glUniformMatrix2dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix2dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix3dv glUniformMatrix3dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix3dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix4dv glUniformMatrix4dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix4dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix2x3dv glUniformMatrix2x3dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix2x3dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix2x4dv glUniformMatrix2x4dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix2x4dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix3x2dv glUniformMatrix3x2dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix3x2dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix3x4dv glUniformMatrix3x4dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix3x4dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix4x2dv glUniformMatrix4x2dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix4x2dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glUniformMatrix4x3dv glUniformMatrix4x3dv<c-o>:echoh HintHL<Bar>echo "void glUniformMatrix4x3dv(GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glGetUniformdv glGetUniformdv<c-o>:echoh HintHL<Bar>echo "void glGetUniformdv(GLuint program, GLint location, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetSubroutineUniformLocation glGetSubroutineUniformLocation<c-o>:echoh HintHL<Bar>echo "GLint glGetSubroutineUniformLocation(GLuint program, GLenum shadertype, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetSubroutineIndex glGetSubroutineIndex<c-o>:echoh HintHL<Bar>echo "GLuint glGetSubroutineIndex(GLuint program, GLenum shadertype, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetActiveSubroutineUniformiv glGetActiveSubroutineUniformiv<c-o>:echoh HintHL<Bar>echo "void glGetActiveSubroutineUniformiv(GLuint program, GLenum shadertype, GLuint index, GLenum pname, GLint *values)"<Bar>echoh None<cr>
inorea glGetActiveSubroutineUniformName glGetActiveSubroutineUniformName<c-o>:echoh HintHL<Bar>echo "void glGetActiveSubroutineUniformName(GLuint program, GLenum shadertype, GLuint index, GLsizei bufsize, GLsizei *length, GLchar *name)"<Bar>echoh None<cr>
inorea glGetActiveSubroutineName glGetActiveSubroutineName<c-o>:echoh HintHL<Bar>echo "void glGetActiveSubroutineName(GLuint program, GLenum shadertype, GLuint index, GLsizei bufsize, GLsizei *length, GLchar *name)"<Bar>echoh None<cr>
inorea glUniformSubroutinesuiv glUniformSubroutinesuiv<c-o>:echoh HintHL<Bar>echo "void glUniformSubroutinesuiv(GLenum shadertype, GLsizei count, const GLuint *indices)"<Bar>echoh None<cr>
inorea glGetUniformSubroutineuiv glGetUniformSubroutineuiv<c-o>:echoh HintHL<Bar>echo "void glGetUniformSubroutineuiv(GLenum shadertype, GLint location, GLuint *params)"<Bar>echoh None<cr>
inorea glGetProgramStageiv glGetProgramStageiv<c-o>:echoh HintHL<Bar>echo "void glGetProgramStageiv(GLuint program, GLenum shadertype, GLenum pname, GLint *values)"<Bar>echoh None<cr>
inorea glPatchParameteri glPatchParameteri<c-o>:echoh HintHL<Bar>echo "void glPatchParameteri(GLenum pname, GLint value)"<Bar>echoh None<cr>
inorea glPatchParameterfv glPatchParameterfv<c-o>:echoh HintHL<Bar>echo "void glPatchParameterfv(GLenum pname, const GLfloat *values)"<Bar>echoh None<cr>
inorea glBindTransformFeedback glBindTransformFeedback<c-o>:echoh HintHL<Bar>echo "void glBindTransformFeedback(GLenum target, GLuint id)"<Bar>echoh None<cr>
inorea glDeleteTransformFeedbacks glDeleteTransformFeedbacks<c-o>:echoh HintHL<Bar>echo "void glDeleteTransformFeedbacks(GLsizei n, const GLuint *ids)"<Bar>echoh None<cr>
inorea glGenTransformFeedbacks glGenTransformFeedbacks<c-o>:echoh HintHL<Bar>echo "void glGenTransformFeedbacks(GLsizei n, GLuint *ids)"<Bar>echoh None<cr>
inorea glIsTransformFeedback glIsTransformFeedback<c-o>:echoh HintHL<Bar>echo "GLboolean glIsTransformFeedback(GLuint id)"<Bar>echoh None<cr>
inorea glPauseTransformFeedback glPauseTransformFeedback<c-o>:echoh HintHL<Bar>echo "void glPauseTransformFeedback(void)"<Bar>echoh None<cr>
inorea glResumeTransformFeedback glResumeTransformFeedback<c-o>:echoh HintHL<Bar>echo "void glResumeTransformFeedback(void)"<Bar>echoh None<cr>
inorea glDrawTransformFeedback glDrawTransformFeedback<c-o>:echoh HintHL<Bar>echo "void glDrawTransformFeedback(GLenum mode, GLuint id)"<Bar>echoh None<cr>
inorea glDrawTransformFeedbackStream glDrawTransformFeedbackStream<c-o>:echoh HintHL<Bar>echo "void glDrawTransformFeedbackStream(GLenum mode, GLuint id, GLuint stream)"<Bar>echoh None<cr>
inorea glBeginQueryIndexed glBeginQueryIndexed<c-o>:echoh HintHL<Bar>echo "void glBeginQueryIndexed(GLenum target, GLuint index, GLuint id)"<Bar>echoh None<cr>
inorea glEndQueryIndexed glEndQueryIndexed<c-o>:echoh HintHL<Bar>echo "void glEndQueryIndexed(GLenum target, GLuint index)"<Bar>echoh None<cr>
inorea glGetQueryIndexediv glGetQueryIndexediv<c-o>:echoh HintHL<Bar>echo "void glGetQueryIndexediv(GLenum target, GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glReleaseShaderCompiler glReleaseShaderCompiler<c-o>:echoh HintHL<Bar>echo "void glReleaseShaderCompiler(void)"<Bar>echoh None<cr>
inorea glShaderBinary glShaderBinary<c-o>:echoh HintHL<Bar>echo "void glShaderBinary(GLsizei count, const GLuint *shaders, GLenum binaryformat, const GLvoid *binary, GLsizei length)"<Bar>echoh None<cr>
inorea glGetShaderPrecisionFormat glGetShaderPrecisionFormat<c-o>:echoh HintHL<Bar>echo "void glGetShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype, GLint *range, GLint *precision)"<Bar>echoh None<cr>
inorea glDepthRangef glDepthRangef<c-o>:echoh HintHL<Bar>echo "void glDepthRangef(GLclampf n, GLclampf f)"<Bar>echoh None<cr>
inorea glClearDepthf glClearDepthf<c-o>:echoh HintHL<Bar>echo "void glClearDepthf(GLclampf d)"<Bar>echoh None<cr>
inorea glGetProgramBinary glGetProgramBinary<c-o>:echoh HintHL<Bar>echo "void glGetProgramBinary(GLuint program, GLsizei bufSize, GLsizei *length, GLenum *binaryFormat, GLvoid *binary)"<Bar>echoh None<cr>
inorea glProgramBinary glProgramBinary<c-o>:echoh HintHL<Bar>echo "void glProgramBinary(GLuint program, GLenum binaryFormat, const GLvoid *binary, GLsizei length)"<Bar>echoh None<cr>
inorea glProgramParameteri glProgramParameteri<c-o>:echoh HintHL<Bar>echo "void glProgramParameteri(GLuint program, GLenum pname, GLint value)"<Bar>echoh None<cr>
inorea glUseProgramStages glUseProgramStages<c-o>:echoh HintHL<Bar>echo "void glUseProgramStages(GLuint pipeline, GLbitfield stages, GLuint program)"<Bar>echoh None<cr>
inorea glActiveShaderProgram glActiveShaderProgram<c-o>:echoh HintHL<Bar>echo "void glActiveShaderProgram(GLuint pipeline, GLuint program)"<Bar>echoh None<cr>
inorea glCreateShaderProgramv glCreateShaderProgramv<c-o>:echoh HintHL<Bar>echo "GLuint glCreateShaderProgramv(GLenum type, GLsizei count, const GLchar* *strings)"<Bar>echoh None<cr>
inorea glBindProgramPipeline glBindProgramPipeline<c-o>:echoh HintHL<Bar>echo "void glBindProgramPipeline(GLuint pipeline)"<Bar>echoh None<cr>
inorea glDeleteProgramPipelines glDeleteProgramPipelines<c-o>:echoh HintHL<Bar>echo "void glDeleteProgramPipelines(GLsizei n, const GLuint *pipelines)"<Bar>echoh None<cr>
inorea glGenProgramPipelines glGenProgramPipelines<c-o>:echoh HintHL<Bar>echo "void glGenProgramPipelines(GLsizei n, GLuint *pipelines)"<Bar>echoh None<cr>
inorea glIsProgramPipeline glIsProgramPipeline<c-o>:echoh HintHL<Bar>echo "GLboolean glIsProgramPipeline(GLuint pipeline)"<Bar>echoh None<cr>
inorea glGetProgramPipelineiv glGetProgramPipelineiv<c-o>:echoh HintHL<Bar>echo "void glGetProgramPipelineiv(GLuint pipeline, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glProgramUniform1i glProgramUniform1i<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1i(GLuint program, GLint location, GLint v0)"<Bar>echoh None<cr>
inorea glProgramUniform1iv glProgramUniform1iv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1iv(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniform1f glProgramUniform1f<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1f(GLuint program, GLint location, GLfloat v0)"<Bar>echoh None<cr>
inorea glProgramUniform1fv glProgramUniform1fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform1d glProgramUniform1d<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1d(GLuint program, GLint location, GLdouble v0)"<Bar>echoh None<cr>
inorea glProgramUniform1dv glProgramUniform1dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniform1ui glProgramUniform1ui<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1ui(GLuint program, GLint location, GLuint v0)"<Bar>echoh None<cr>
inorea glProgramUniform1uiv glProgramUniform1uiv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glProgramUniform2i glProgramUniform2i<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2i(GLuint program, GLint location, GLint v0, GLint v1)"<Bar>echoh None<cr>
inorea glProgramUniform2iv glProgramUniform2iv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2iv(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniform2f glProgramUniform2f<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2f(GLuint program, GLint location, GLfloat v0, GLfloat v1)"<Bar>echoh None<cr>
inorea glProgramUniform2fv glProgramUniform2fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform2d glProgramUniform2d<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2d(GLuint program, GLint location, GLdouble v0, GLdouble v1)"<Bar>echoh None<cr>
inorea glProgramUniform2dv glProgramUniform2dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniform2ui glProgramUniform2ui<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2ui(GLuint program, GLint location, GLuint v0, GLuint v1)"<Bar>echoh None<cr>
inorea glProgramUniform2uiv glProgramUniform2uiv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glProgramUniform3i glProgramUniform3i<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3i(GLuint program, GLint location, GLint v0, GLint v1, GLint v2)"<Bar>echoh None<cr>
inorea glProgramUniform3iv glProgramUniform3iv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3iv(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniform3f glProgramUniform3f<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3f(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2)"<Bar>echoh None<cr>
inorea glProgramUniform3fv glProgramUniform3fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform3d glProgramUniform3d<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3d(GLuint program, GLint location, GLdouble v0, GLdouble v1, GLdouble v2)"<Bar>echoh None<cr>
inorea glProgramUniform3dv glProgramUniform3dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniform3ui glProgramUniform3ui<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3ui(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2)"<Bar>echoh None<cr>
inorea glProgramUniform3uiv glProgramUniform3uiv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glProgramUniform4i glProgramUniform4i<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4i(GLuint program, GLint location, GLint v0, GLint v1, GLint v2, GLint v3)"<Bar>echoh None<cr>
inorea glProgramUniform4iv glProgramUniform4iv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4iv(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniform4f glProgramUniform4f<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4f(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)"<Bar>echoh None<cr>
inorea glProgramUniform4fv glProgramUniform4fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4fv(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform4d glProgramUniform4d<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4d(GLuint program, GLint location, GLdouble v0, GLdouble v1, GLdouble v2, GLdouble v3)"<Bar>echoh None<cr>
inorea glProgramUniform4dv glProgramUniform4dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4dv(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniform4ui glProgramUniform4ui<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4ui(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3)"<Bar>echoh None<cr>
inorea glProgramUniform4uiv glProgramUniform4uiv<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4uiv(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2fv glProgramUniformMatrix2fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3fv glProgramUniformMatrix3fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4fv glProgramUniformMatrix4fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2dv glProgramUniformMatrix2dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3dv glProgramUniformMatrix3dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4dv glProgramUniformMatrix4dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x3fv glProgramUniformMatrix2x3fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x3fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x2fv glProgramUniformMatrix3x2fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x2fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x4fv glProgramUniformMatrix2x4fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x4fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x2fv glProgramUniformMatrix4x2fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x2fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x4fv glProgramUniformMatrix3x4fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x4fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x3fv glProgramUniformMatrix4x3fv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x3fv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x3dv glProgramUniformMatrix2x3dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x3dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x2dv glProgramUniformMatrix3x2dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x2dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x4dv glProgramUniformMatrix2x4dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x4dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x2dv glProgramUniformMatrix4x2dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x2dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x4dv glProgramUniformMatrix3x4dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x4dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x3dv glProgramUniformMatrix4x3dv<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x3dv(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glValidateProgramPipeline glValidateProgramPipeline<c-o>:echoh HintHL<Bar>echo "void glValidateProgramPipeline(GLuint pipeline)"<Bar>echoh None<cr>
inorea glGetProgramPipelineInfoLog glGetProgramPipelineInfoLog<c-o>:echoh HintHL<Bar>echo "void glGetProgramPipelineInfoLog(GLuint pipeline, GLsizei bufSize, GLsizei *length, GLchar *infoLog)"<Bar>echoh None<cr>
inorea glVertexAttribL1d glVertexAttribL1d<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1d(GLuint index, GLdouble x)"<Bar>echoh None<cr>
inorea glVertexAttribL2d glVertexAttribL2d<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2d(GLuint index, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertexAttribL3d glVertexAttribL3d<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3d(GLuint index, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertexAttribL4d glVertexAttribL4d<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4d(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertexAttribL1dv glVertexAttribL1dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribL2dv glVertexAttribL2dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribL3dv glVertexAttribL3dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribL4dv glVertexAttribL4dv<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4dv(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribLPointer glVertexAttribLPointer<c-o>:echoh HintHL<Bar>echo "void glVertexAttribLPointer(GLuint index, GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glGetVertexAttribLdv glGetVertexAttribLdv<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribLdv(GLuint index, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glViewportArrayv glViewportArrayv<c-o>:echoh HintHL<Bar>echo "void glViewportArrayv(GLuint first, GLsizei count, const GLfloat *v)"<Bar>echoh None<cr>
inorea glViewportIndexedf glViewportIndexedf<c-o>:echoh HintHL<Bar>echo "void glViewportIndexedf(GLuint index, GLfloat x, GLfloat y, GLfloat w, GLfloat h)"<Bar>echoh None<cr>
inorea glViewportIndexedfv glViewportIndexedfv<c-o>:echoh HintHL<Bar>echo "void glViewportIndexedfv(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glScissorArrayv glScissorArrayv<c-o>:echoh HintHL<Bar>echo "void glScissorArrayv(GLuint first, GLsizei count, const GLint *v)"<Bar>echoh None<cr>
inorea glScissorIndexed glScissorIndexed<c-o>:echoh HintHL<Bar>echo "void glScissorIndexed(GLuint index, GLint left, GLint bottom, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glScissorIndexedv glScissorIndexedv<c-o>:echoh HintHL<Bar>echo "void glScissorIndexedv(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glDepthRangeArrayv glDepthRangeArrayv<c-o>:echoh HintHL<Bar>echo "void glDepthRangeArrayv(GLuint first, GLsizei count, const GLclampd *v)"<Bar>echoh None<cr>
inorea glDepthRangeIndexed glDepthRangeIndexed<c-o>:echoh HintHL<Bar>echo "void glDepthRangeIndexed(GLuint index, GLclampd n, GLclampd f)"<Bar>echoh None<cr>
inorea glGetFloati_v glGetFloati_v<c-o>:echoh HintHL<Bar>echo "void glGetFloati_v(GLenum target, GLuint index, GLfloat *data)"<Bar>echoh None<cr>
inorea glGetDoublei_v glGetDoublei_v<c-o>:echoh HintHL<Bar>echo "void glGetDoublei_v(GLenum target, GLuint index, GLdouble *data)"<Bar>echoh None<cr>
inorea glCreateSyncFromCLeventARB glCreateSyncFromCLeventARB<c-o>:echoh HintHL<Bar>echo "GLsync glCreateSyncFromCLeventARB(struct _cl_context * context, struct _cl_event * event, GLbitfield flags)"<Bar>echoh None<cr>
inorea glDebugMessageControlARB glDebugMessageControlARB<c-o>:echoh HintHL<Bar>echo "void glDebugMessageControlARB(GLenum source, GLenum type, GLenum severity, GLsizei count, const GLuint *ids, GLboolean enabled)"<Bar>echoh None<cr>
inorea glDebugMessageInsertARB glDebugMessageInsertARB<c-o>:echoh HintHL<Bar>echo "void glDebugMessageInsertARB(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *buf)"<Bar>echoh None<cr>
inorea glDebugMessageCallbackARB glDebugMessageCallbackARB<c-o>:echoh HintHL<Bar>echo "void glDebugMessageCallbackARB(GLDEBUGPROCARB callback, const GLvoid *userParam)"<Bar>echoh None<cr>
inorea glGetDebugMessageLogARB glGetDebugMessageLogARB<c-o>:echoh HintHL<Bar>echo "GLuint glGetDebugMessageLogARB(GLuint count, GLsizei bufsize, GLenum *sources, GLenum *types, GLuint *ids, GLenum *severities, GLsizei *lengths, GLchar *messageLog)"<Bar>echoh None<cr>
inorea glGetGraphicsResetStatusARB glGetGraphicsResetStatusARB<c-o>:echoh HintHL<Bar>echo "GLenum glGetGraphicsResetStatusARB(void)"<Bar>echoh None<cr>
inorea glGetnMapdvARB glGetnMapdvARB<c-o>:echoh HintHL<Bar>echo "void glGetnMapdvARB(GLenum target, GLenum query, GLsizei bufSize, GLdouble *v)"<Bar>echoh None<cr>
inorea glGetnMapfvARB glGetnMapfvARB<c-o>:echoh HintHL<Bar>echo "void glGetnMapfvARB(GLenum target, GLenum query, GLsizei bufSize, GLfloat *v)"<Bar>echoh None<cr>
inorea glGetnMapivARB glGetnMapivARB<c-o>:echoh HintHL<Bar>echo "void glGetnMapivARB(GLenum target, GLenum query, GLsizei bufSize, GLint *v)"<Bar>echoh None<cr>
inorea glGetnPixelMapfvARB glGetnPixelMapfvARB<c-o>:echoh HintHL<Bar>echo "void glGetnPixelMapfvARB(GLenum map, GLsizei bufSize, GLfloat *values)"<Bar>echoh None<cr>
inorea glGetnPixelMapuivARB glGetnPixelMapuivARB<c-o>:echoh HintHL<Bar>echo "void glGetnPixelMapuivARB(GLenum map, GLsizei bufSize, GLuint *values)"<Bar>echoh None<cr>
inorea glGetnPixelMapusvARB glGetnPixelMapusvARB<c-o>:echoh HintHL<Bar>echo "void glGetnPixelMapusvARB(GLenum map, GLsizei bufSize, GLushort *values)"<Bar>echoh None<cr>
inorea glGetnPolygonStippleARB glGetnPolygonStippleARB<c-o>:echoh HintHL<Bar>echo "void glGetnPolygonStippleARB(GLsizei bufSize, GLubyte *pattern)"<Bar>echoh None<cr>
inorea glGetnColorTableARB glGetnColorTableARB<c-o>:echoh HintHL<Bar>echo "void glGetnColorTableARB(GLenum target, GLenum format, GLenum type, GLsizei bufSize, GLvoid *table)"<Bar>echoh None<cr>
inorea glGetnConvolutionFilterARB glGetnConvolutionFilterARB<c-o>:echoh HintHL<Bar>echo "void glGetnConvolutionFilterARB(GLenum target, GLenum format, GLenum type, GLsizei bufSize, GLvoid *image)"<Bar>echoh None<cr>
inorea glGetnSeparableFilterARB glGetnSeparableFilterARB<c-o>:echoh HintHL<Bar>echo "void glGetnSeparableFilterARB(GLenum target, GLenum format, GLenum type, GLsizei rowBufSize, GLvoid *row, GLsizei columnBufSize, GLvoid *column, GLvoid *span)"<Bar>echoh None<cr>
inorea glGetnHistogramARB glGetnHistogramARB<c-o>:echoh HintHL<Bar>echo "void glGetnHistogramARB(GLenum target, GLboolean reset, GLenum format, GLenum type, GLsizei bufSize, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetnMinmaxARB glGetnMinmaxARB<c-o>:echoh HintHL<Bar>echo "void glGetnMinmaxARB(GLenum target, GLboolean reset, GLenum format, GLenum type, GLsizei bufSize, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetnTexImageARB glGetnTexImageARB<c-o>:echoh HintHL<Bar>echo "void glGetnTexImageARB(GLenum target, GLint level, GLenum format, GLenum type, GLsizei bufSize, GLvoid *img)"<Bar>echoh None<cr>
inorea glReadnPixelsARB glReadnPixelsARB<c-o>:echoh HintHL<Bar>echo "void glReadnPixelsARB(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLsizei bufSize, GLvoid *data)"<Bar>echoh None<cr>
inorea glGetnCompressedTexImageARB glGetnCompressedTexImageARB<c-o>:echoh HintHL<Bar>echo "void glGetnCompressedTexImageARB(GLenum target, GLint lod, GLsizei bufSize, GLvoid *img)"<Bar>echoh None<cr>
inorea glGetnUniformfvARB glGetnUniformfvARB<c-o>:echoh HintHL<Bar>echo "void glGetnUniformfvARB(GLuint program, GLint location, GLsizei bufSize, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetnUniformivARB glGetnUniformivARB<c-o>:echoh HintHL<Bar>echo "void glGetnUniformivARB(GLuint program, GLint location, GLsizei bufSize, GLint *params)"<Bar>echoh None<cr>
inorea glGetnUniformuivARB glGetnUniformuivARB<c-o>:echoh HintHL<Bar>echo "void glGetnUniformuivARB(GLuint program, GLint location, GLsizei bufSize, GLuint *params)"<Bar>echoh None<cr>
inorea glGetnUniformdvARB glGetnUniformdvARB<c-o>:echoh HintHL<Bar>echo "void glGetnUniformdvARB(GLuint program, GLint location, GLsizei bufSize, GLdouble *params)"<Bar>echoh None<cr>
inorea glBlendColorEXT glBlendColorEXT<c-o>:echoh HintHL<Bar>echo "void glBlendColorEXT(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"<Bar>echoh None<cr>
inorea glPolygonOffsetEXT glPolygonOffsetEXT<c-o>:echoh HintHL<Bar>echo "void glPolygonOffsetEXT(GLfloat factor, GLfloat bias)"<Bar>echoh None<cr>
inorea glTexImage3DEXT glTexImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glTexImage3DEXT(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTexSubImage3DEXT glTexSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glTexSubImage3DEXT(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glGetTexFilterFuncSGIS glGetTexFilterFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glGetTexFilterFuncSGIS(GLenum target, GLenum filter, GLfloat *weights)"<Bar>echoh None<cr>
inorea glTexFilterFuncSGIS glTexFilterFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glTexFilterFuncSGIS(GLenum target, GLenum filter, GLsizei n, const GLfloat *weights)"<Bar>echoh None<cr>
inorea glTexSubImage1DEXT glTexSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glTexSubImage1DEXT(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTexSubImage2DEXT glTexSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glTexSubImage2DEXT(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glCopyTexImage1DEXT glCopyTexImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTexImage1DEXT(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)"<Bar>echoh None<cr>
inorea glCopyTexImage2DEXT glCopyTexImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTexImage2DEXT(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)"<Bar>echoh None<cr>
inorea glCopyTexSubImage1DEXT glCopyTexSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage1DEXT(GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyTexSubImage2DEXT glCopyTexSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage2DEXT(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glCopyTexSubImage3DEXT glCopyTexSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage3DEXT(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetHistogramEXT glGetHistogramEXT<c-o>:echoh HintHL<Bar>echo "void glGetHistogramEXT(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetHistogramParameterfvEXT glGetHistogramParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetHistogramParameterfvEXT(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetHistogramParameterivEXT glGetHistogramParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetHistogramParameterivEXT(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMinmaxEXT glGetMinmaxEXT<c-o>:echoh HintHL<Bar>echo "void glGetMinmaxEXT(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetMinmaxParameterfvEXT glGetMinmaxParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetMinmaxParameterfvEXT(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMinmaxParameterivEXT glGetMinmaxParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetMinmaxParameterivEXT(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glHistogramEXT glHistogramEXT<c-o>:echoh HintHL<Bar>echo "void glHistogramEXT(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink)"<Bar>echoh None<cr>
inorea glMinmaxEXT glMinmaxEXT<c-o>:echoh HintHL<Bar>echo "void glMinmaxEXT(GLenum target, GLenum internalformat, GLboolean sink)"<Bar>echoh None<cr>
inorea glResetHistogramEXT glResetHistogramEXT<c-o>:echoh HintHL<Bar>echo "void glResetHistogramEXT(GLenum target)"<Bar>echoh None<cr>
inorea glResetMinmaxEXT glResetMinmaxEXT<c-o>:echoh HintHL<Bar>echo "void glResetMinmaxEXT(GLenum target)"<Bar>echoh None<cr>
inorea glConvolutionFilter1DEXT glConvolutionFilter1DEXT<c-o>:echoh HintHL<Bar>echo "void glConvolutionFilter1DEXT(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *image)"<Bar>echoh None<cr>
inorea glConvolutionFilter2DEXT glConvolutionFilter2DEXT<c-o>:echoh HintHL<Bar>echo "void glConvolutionFilter2DEXT(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *image)"<Bar>echoh None<cr>
inorea glConvolutionParameterfEXT glConvolutionParameterfEXT<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameterfEXT(GLenum target, GLenum pname, GLfloat params)"<Bar>echoh None<cr>
inorea glConvolutionParameterfvEXT glConvolutionParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameterfvEXT(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glConvolutionParameteriEXT glConvolutionParameteriEXT<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameteriEXT(GLenum target, GLenum pname, GLint params)"<Bar>echoh None<cr>
inorea glConvolutionParameterivEXT glConvolutionParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameterivEXT(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glCopyConvolutionFilter1DEXT glCopyConvolutionFilter1DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyConvolutionFilter1DEXT(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyConvolutionFilter2DEXT glCopyConvolutionFilter2DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyConvolutionFilter2DEXT(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetConvolutionFilterEXT glGetConvolutionFilterEXT<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionFilterEXT(GLenum target, GLenum format, GLenum type, GLvoid *image)"<Bar>echoh None<cr>
inorea glGetConvolutionParameterfvEXT glGetConvolutionParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionParameterfvEXT(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetConvolutionParameterivEXT glGetConvolutionParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionParameterivEXT(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetSeparableFilterEXT glGetSeparableFilterEXT<c-o>:echoh HintHL<Bar>echo "void glGetSeparableFilterEXT(GLenum target, GLenum format, GLenum type, GLvoid *row, GLvoid *column, GLvoid *span)"<Bar>echoh None<cr>
inorea glSeparableFilter2DEXT glSeparableFilter2DEXT<c-o>:echoh HintHL<Bar>echo "void glSeparableFilter2DEXT(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *row, const GLvoid *column)"<Bar>echoh None<cr>
inorea glColorTableSGI glColorTableSGI<c-o>:echoh HintHL<Bar>echo "void glColorTableSGI(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *table)"<Bar>echoh None<cr>
inorea glColorTableParameterfvSGI glColorTableParameterfvSGI<c-o>:echoh HintHL<Bar>echo "void glColorTableParameterfvSGI(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glColorTableParameterivSGI glColorTableParameterivSGI<c-o>:echoh HintHL<Bar>echo "void glColorTableParameterivSGI(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glCopyColorTableSGI glCopyColorTableSGI<c-o>:echoh HintHL<Bar>echo "void glCopyColorTableSGI(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glGetColorTableSGI glGetColorTableSGI<c-o>:echoh HintHL<Bar>echo "void glGetColorTableSGI(GLenum target, GLenum format, GLenum type, GLvoid *table)"<Bar>echoh None<cr>
inorea glGetColorTableParameterfvSGI glGetColorTableParameterfvSGI<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameterfvSGI(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetColorTableParameterivSGI glGetColorTableParameterivSGI<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameterivSGI(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glPixelTexGenSGIX glPixelTexGenSGIX<c-o>:echoh HintHL<Bar>echo "void glPixelTexGenSGIX(GLenum mode)"<Bar>echoh None<cr>
inorea glPixelTexGenParameteriSGIS glPixelTexGenParameteriSGIS<c-o>:echoh HintHL<Bar>echo "void glPixelTexGenParameteriSGIS(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPixelTexGenParameterivSGIS glPixelTexGenParameterivSGIS<c-o>:echoh HintHL<Bar>echo "void glPixelTexGenParameterivSGIS(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glPixelTexGenParameterfSGIS glPixelTexGenParameterfSGIS<c-o>:echoh HintHL<Bar>echo "void glPixelTexGenParameterfSGIS(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPixelTexGenParameterfvSGIS glPixelTexGenParameterfvSGIS<c-o>:echoh HintHL<Bar>echo "void glPixelTexGenParameterfvSGIS(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetPixelTexGenParameterivSGIS glGetPixelTexGenParameterivSGIS<c-o>:echoh HintHL<Bar>echo "void glGetPixelTexGenParameterivSGIS(GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetPixelTexGenParameterfvSGIS glGetPixelTexGenParameterfvSGIS<c-o>:echoh HintHL<Bar>echo "void glGetPixelTexGenParameterfvSGIS(GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glTexImage4DSGIS glTexImage4DSGIS<c-o>:echoh HintHL<Bar>echo "void glTexImage4DSGIS(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLsizei size4d, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTexSubImage4DSGIS glTexSubImage4DSGIS<c-o>:echoh HintHL<Bar>echo "void glTexSubImage4DSGIS(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint woffset, GLsizei width, GLsizei height, GLsizei depth, GLsizei size4d, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glAreTexturesResidentEXT glAreTexturesResidentEXT<c-o>:echoh HintHL<Bar>echo "GLboolean glAreTexturesResidentEXT(GLsizei n, const GLuint *textures, GLboolean *residences)"<Bar>echoh None<cr>
inorea glBindTextureEXT glBindTextureEXT<c-o>:echoh HintHL<Bar>echo "void glBindTextureEXT(GLenum target, GLuint texture)"<Bar>echoh None<cr>
inorea glDeleteTexturesEXT glDeleteTexturesEXT<c-o>:echoh HintHL<Bar>echo "void glDeleteTexturesEXT(GLsizei n, const GLuint *textures)"<Bar>echoh None<cr>
inorea glGenTexturesEXT glGenTexturesEXT<c-o>:echoh HintHL<Bar>echo "void glGenTexturesEXT(GLsizei n, GLuint *textures)"<Bar>echoh None<cr>
inorea glIsTextureEXT glIsTextureEXT<c-o>:echoh HintHL<Bar>echo "GLboolean glIsTextureEXT(GLuint texture)"<Bar>echoh None<cr>
inorea glPrioritizeTexturesEXT glPrioritizeTexturesEXT<c-o>:echoh HintHL<Bar>echo "void glPrioritizeTexturesEXT(GLsizei n, const GLuint *textures, const GLclampf *priorities)"<Bar>echoh None<cr>
inorea glDetailTexFuncSGIS glDetailTexFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glDetailTexFuncSGIS(GLenum target, GLsizei n, const GLfloat *points)"<Bar>echoh None<cr>
inorea glGetDetailTexFuncSGIS glGetDetailTexFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glGetDetailTexFuncSGIS(GLenum target, GLfloat *points)"<Bar>echoh None<cr>
inorea glSharpenTexFuncSGIS glSharpenTexFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glSharpenTexFuncSGIS(GLenum target, GLsizei n, const GLfloat *points)"<Bar>echoh None<cr>
inorea glGetSharpenTexFuncSGIS glGetSharpenTexFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glGetSharpenTexFuncSGIS(GLenum target, GLfloat *points)"<Bar>echoh None<cr>
inorea glSampleMaskSGIS glSampleMaskSGIS<c-o>:echoh HintHL<Bar>echo "void glSampleMaskSGIS(GLclampf value, GLboolean invert)"<Bar>echoh None<cr>
inorea glSamplePatternSGIS glSamplePatternSGIS<c-o>:echoh HintHL<Bar>echo "void glSamplePatternSGIS(GLenum pattern)"<Bar>echoh None<cr>
inorea glArrayElementEXT glArrayElementEXT<c-o>:echoh HintHL<Bar>echo "void glArrayElementEXT(GLint i)"<Bar>echoh None<cr>
inorea glColorPointerEXT glColorPointerEXT<c-o>:echoh HintHL<Bar>echo "void glColorPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glDrawArraysEXT glDrawArraysEXT<c-o>:echoh HintHL<Bar>echo "void glDrawArraysEXT(GLenum mode, GLint first, GLsizei count)"<Bar>echoh None<cr>
inorea glEdgeFlagPointerEXT glEdgeFlagPointerEXT<c-o>:echoh HintHL<Bar>echo "void glEdgeFlagPointerEXT(GLsizei stride, GLsizei count, const GLboolean *pointer)"<Bar>echoh None<cr>
inorea glGetPointervEXT glGetPointervEXT<c-o>:echoh HintHL<Bar>echo "void glGetPointervEXT(GLenum pname, GLvoid* *params)"<Bar>echoh None<cr>
inorea glIndexPointerEXT glIndexPointerEXT<c-o>:echoh HintHL<Bar>echo "void glIndexPointerEXT(GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glNormalPointerEXT glNormalPointerEXT<c-o>:echoh HintHL<Bar>echo "void glNormalPointerEXT(GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glTexCoordPointerEXT glTexCoordPointerEXT<c-o>:echoh HintHL<Bar>echo "void glTexCoordPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glVertexPointerEXT glVertexPointerEXT<c-o>:echoh HintHL<Bar>echo "void glVertexPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glBlendEquationEXT glBlendEquationEXT<c-o>:echoh HintHL<Bar>echo "void glBlendEquationEXT(GLenum mode)"<Bar>echoh None<cr>
inorea glSpriteParameterfSGIX glSpriteParameterfSGIX<c-o>:echoh HintHL<Bar>echo "void glSpriteParameterfSGIX(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glSpriteParameterfvSGIX glSpriteParameterfvSGIX<c-o>:echoh HintHL<Bar>echo "void glSpriteParameterfvSGIX(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glSpriteParameteriSGIX glSpriteParameteriSGIX<c-o>:echoh HintHL<Bar>echo "void glSpriteParameteriSGIX(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glSpriteParameterivSGIX glSpriteParameterivSGIX<c-o>:echoh HintHL<Bar>echo "void glSpriteParameterivSGIX(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glPointParameterfEXT glPointParameterfEXT<c-o>:echoh HintHL<Bar>echo "void glPointParameterfEXT(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPointParameterfvEXT glPointParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glPointParameterfvEXT(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glPointParameterfSGIS glPointParameterfSGIS<c-o>:echoh HintHL<Bar>echo "void glPointParameterfSGIS(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPointParameterfvSGIS glPointParameterfvSGIS<c-o>:echoh HintHL<Bar>echo "void glPointParameterfvSGIS(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetInstrumentsSGIX glGetInstrumentsSGIX<c-o>:echoh HintHL<Bar>echo "GLint glGetInstrumentsSGIX(void)"<Bar>echoh None<cr>
inorea glInstrumentsBufferSGIX glInstrumentsBufferSGIX<c-o>:echoh HintHL<Bar>echo "void glInstrumentsBufferSGIX(GLsizei size, GLint *buffer)"<Bar>echoh None<cr>
inorea glPollInstrumentsSGIX glPollInstrumentsSGIX<c-o>:echoh HintHL<Bar>echo "GLint glPollInstrumentsSGIX(GLint *marker_p)"<Bar>echoh None<cr>
inorea glReadInstrumentsSGIX glReadInstrumentsSGIX<c-o>:echoh HintHL<Bar>echo "void glReadInstrumentsSGIX(GLint marker)"<Bar>echoh None<cr>
inorea glStartInstrumentsSGIX glStartInstrumentsSGIX<c-o>:echoh HintHL<Bar>echo "void glStartInstrumentsSGIX(void)"<Bar>echoh None<cr>
inorea glStopInstrumentsSGIX glStopInstrumentsSGIX<c-o>:echoh HintHL<Bar>echo "void glStopInstrumentsSGIX(GLint marker)"<Bar>echoh None<cr>
inorea glFrameZoomSGIX glFrameZoomSGIX<c-o>:echoh HintHL<Bar>echo "void glFrameZoomSGIX(GLint factor)"<Bar>echoh None<cr>
inorea glTagSampleBufferSGIX glTagSampleBufferSGIX<c-o>:echoh HintHL<Bar>echo "void glTagSampleBufferSGIX(void)"<Bar>echoh None<cr>
inorea glDeformationMap3dSGIX glDeformationMap3dSGIX<c-o>:echoh HintHL<Bar>echo "void glDeformationMap3dSGIX(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, GLdouble w1, GLdouble w2, GLint wstride, GLint worder, const GLdouble *points)"<Bar>echoh None<cr>
inorea glDeformationMap3fSGIX glDeformationMap3fSGIX<c-o>:echoh HintHL<Bar>echo "void glDeformationMap3fSGIX(GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, GLfloat w1, GLfloat w2, GLint wstride, GLint worder, const GLfloat *points)"<Bar>echoh None<cr>
inorea glDeformSGIX glDeformSGIX<c-o>:echoh HintHL<Bar>echo "void glDeformSGIX(GLbitfield mask)"<Bar>echoh None<cr>
inorea glLoadIdentityDeformationMapSGIX glLoadIdentityDeformationMapSGIX<c-o>:echoh HintHL<Bar>echo "void glLoadIdentityDeformationMapSGIX(GLbitfield mask)"<Bar>echoh None<cr>
inorea glReferencePlaneSGIX glReferencePlaneSGIX<c-o>:echoh HintHL<Bar>echo "void glReferencePlaneSGIX(const GLdouble *equation)"<Bar>echoh None<cr>
inorea glFlushRasterSGIX glFlushRasterSGIX<c-o>:echoh HintHL<Bar>echo "void glFlushRasterSGIX(void)"<Bar>echoh None<cr>
inorea glFogFuncSGIS glFogFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glFogFuncSGIS(GLsizei n, const GLfloat *points)"<Bar>echoh None<cr>
inorea glGetFogFuncSGIS glGetFogFuncSGIS<c-o>:echoh HintHL<Bar>echo "void glGetFogFuncSGIS(GLfloat *points)"<Bar>echoh None<cr>
inorea glImageTransformParameteriHP glImageTransformParameteriHP<c-o>:echoh HintHL<Bar>echo "void glImageTransformParameteriHP(GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glImageTransformParameterfHP glImageTransformParameterfHP<c-o>:echoh HintHL<Bar>echo "void glImageTransformParameterfHP(GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glImageTransformParameterivHP glImageTransformParameterivHP<c-o>:echoh HintHL<Bar>echo "void glImageTransformParameterivHP(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glImageTransformParameterfvHP glImageTransformParameterfvHP<c-o>:echoh HintHL<Bar>echo "void glImageTransformParameterfvHP(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetImageTransformParameterivHP glGetImageTransformParameterivHP<c-o>:echoh HintHL<Bar>echo "void glGetImageTransformParameterivHP(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetImageTransformParameterfvHP glGetImageTransformParameterfvHP<c-o>:echoh HintHL<Bar>echo "void glGetImageTransformParameterfvHP(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glColorSubTableEXT glColorSubTableEXT<c-o>:echoh HintHL<Bar>echo "void glColorSubTableEXT(GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCopyColorSubTableEXT glCopyColorSubTableEXT<c-o>:echoh HintHL<Bar>echo "void glCopyColorSubTableEXT(GLenum target, GLsizei start, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glHintPGI glHintPGI<c-o>:echoh HintHL<Bar>echo "void glHintPGI(GLenum target, GLint mode)"<Bar>echoh None<cr>
inorea glColorTableEXT glColorTableEXT<c-o>:echoh HintHL<Bar>echo "void glColorTableEXT(GLenum target, GLenum internalFormat, GLsizei width, GLenum format, GLenum type, const GLvoid *table)"<Bar>echoh None<cr>
inorea glGetColorTableEXT glGetColorTableEXT<c-o>:echoh HintHL<Bar>echo "void glGetColorTableEXT(GLenum target, GLenum format, GLenum type, GLvoid *data)"<Bar>echoh None<cr>
inorea glGetColorTableParameterivEXT glGetColorTableParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameterivEXT(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetColorTableParameterfvEXT glGetColorTableParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameterfvEXT(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetListParameterfvSGIX glGetListParameterfvSGIX<c-o>:echoh HintHL<Bar>echo "void glGetListParameterfvSGIX(GLuint list, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetListParameterivSGIX glGetListParameterivSGIX<c-o>:echoh HintHL<Bar>echo "void glGetListParameterivSGIX(GLuint list, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glListParameterfSGIX glListParameterfSGIX<c-o>:echoh HintHL<Bar>echo "void glListParameterfSGIX(GLuint list, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glListParameterfvSGIX glListParameterfvSGIX<c-o>:echoh HintHL<Bar>echo "void glListParameterfvSGIX(GLuint list, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glListParameteriSGIX glListParameteriSGIX<c-o>:echoh HintHL<Bar>echo "void glListParameteriSGIX(GLuint list, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glListParameterivSGIX glListParameterivSGIX<c-o>:echoh HintHL<Bar>echo "void glListParameterivSGIX(GLuint list, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glIndexMaterialEXT glIndexMaterialEXT<c-o>:echoh HintHL<Bar>echo "void glIndexMaterialEXT(GLenum face, GLenum mode)"<Bar>echoh None<cr>
inorea glIndexFuncEXT glIndexFuncEXT<c-o>:echoh HintHL<Bar>echo "void glIndexFuncEXT(GLenum func, GLclampf ref)"<Bar>echoh None<cr>
inorea glLockArraysEXT glLockArraysEXT<c-o>:echoh HintHL<Bar>echo "void glLockArraysEXT(GLint first, GLsizei count)"<Bar>echoh None<cr>
inorea glUnlockArraysEXT glUnlockArraysEXT<c-o>:echoh HintHL<Bar>echo "void glUnlockArraysEXT(void)"<Bar>echoh None<cr>
inorea glCullParameterdvEXT glCullParameterdvEXT<c-o>:echoh HintHL<Bar>echo "void glCullParameterdvEXT(GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glCullParameterfvEXT glCullParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glCullParameterfvEXT(GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glFragmentColorMaterialSGIX glFragmentColorMaterialSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentColorMaterialSGIX(GLenum face, GLenum mode)"<Bar>echoh None<cr>
inorea glFragmentLightfSGIX glFragmentLightfSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightfSGIX(GLenum light, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glFragmentLightfvSGIX glFragmentLightfvSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightfvSGIX(GLenum light, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glFragmentLightiSGIX glFragmentLightiSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightiSGIX(GLenum light, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glFragmentLightivSGIX glFragmentLightivSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightivSGIX(GLenum light, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glFragmentLightModelfSGIX glFragmentLightModelfSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightModelfSGIX(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glFragmentLightModelfvSGIX glFragmentLightModelfvSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightModelfvSGIX(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glFragmentLightModeliSGIX glFragmentLightModeliSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightModeliSGIX(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glFragmentLightModelivSGIX glFragmentLightModelivSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentLightModelivSGIX(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glFragmentMaterialfSGIX glFragmentMaterialfSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentMaterialfSGIX(GLenum face, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glFragmentMaterialfvSGIX glFragmentMaterialfvSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentMaterialfvSGIX(GLenum face, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glFragmentMaterialiSGIX glFragmentMaterialiSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentMaterialiSGIX(GLenum face, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glFragmentMaterialivSGIX glFragmentMaterialivSGIX<c-o>:echoh HintHL<Bar>echo "void glFragmentMaterialivSGIX(GLenum face, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glGetFragmentLightfvSGIX glGetFragmentLightfvSGIX<c-o>:echoh HintHL<Bar>echo "void glGetFragmentLightfvSGIX(GLenum light, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetFragmentLightivSGIX glGetFragmentLightivSGIX<c-o>:echoh HintHL<Bar>echo "void glGetFragmentLightivSGIX(GLenum light, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetFragmentMaterialfvSGIX glGetFragmentMaterialfvSGIX<c-o>:echoh HintHL<Bar>echo "void glGetFragmentMaterialfvSGIX(GLenum face, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetFragmentMaterialivSGIX glGetFragmentMaterialivSGIX<c-o>:echoh HintHL<Bar>echo "void glGetFragmentMaterialivSGIX(GLenum face, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glLightEnviSGIX glLightEnviSGIX<c-o>:echoh HintHL<Bar>echo "void glLightEnviSGIX(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glDrawRangeElementsEXT glDrawRangeElementsEXT<c-o>:echoh HintHL<Bar>echo "void glDrawRangeElementsEXT(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices)"<Bar>echoh None<cr>
inorea glApplyTextureEXT glApplyTextureEXT<c-o>:echoh HintHL<Bar>echo "void glApplyTextureEXT(GLenum mode)"<Bar>echoh None<cr>
inorea glTextureLightEXT glTextureLightEXT<c-o>:echoh HintHL<Bar>echo "void glTextureLightEXT(GLenum pname)"<Bar>echoh None<cr>
inorea glTextureMaterialEXT glTextureMaterialEXT<c-o>:echoh HintHL<Bar>echo "void glTextureMaterialEXT(GLenum face, GLenum mode)"<Bar>echoh None<cr>
inorea glAsyncMarkerSGIX glAsyncMarkerSGIX<c-o>:echoh HintHL<Bar>echo "void glAsyncMarkerSGIX(GLuint marker)"<Bar>echoh None<cr>
inorea glFinishAsyncSGIX glFinishAsyncSGIX<c-o>:echoh HintHL<Bar>echo "GLint glFinishAsyncSGIX(GLuint *markerp)"<Bar>echoh None<cr>
inorea glPollAsyncSGIX glPollAsyncSGIX<c-o>:echoh HintHL<Bar>echo "GLint glPollAsyncSGIX(GLuint *markerp)"<Bar>echoh None<cr>
inorea glGenAsyncMarkersSGIX glGenAsyncMarkersSGIX<c-o>:echoh HintHL<Bar>echo "GLuint glGenAsyncMarkersSGIX(GLsizei range)"<Bar>echoh None<cr>
inorea glDeleteAsyncMarkersSGIX glDeleteAsyncMarkersSGIX<c-o>:echoh HintHL<Bar>echo "void glDeleteAsyncMarkersSGIX(GLuint marker, GLsizei range)"<Bar>echoh None<cr>
inorea glIsAsyncMarkerSGIX glIsAsyncMarkerSGIX<c-o>:echoh HintHL<Bar>echo "GLboolean glIsAsyncMarkerSGIX(GLuint marker)"<Bar>echoh None<cr>
inorea glVertexPointervINTEL glVertexPointervINTEL<c-o>:echoh HintHL<Bar>echo "void glVertexPointervINTEL(GLint size, GLenum type, const GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glNormalPointervINTEL glNormalPointervINTEL<c-o>:echoh HintHL<Bar>echo "void glNormalPointervINTEL(GLenum type, const GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glColorPointervINTEL glColorPointervINTEL<c-o>:echoh HintHL<Bar>echo "void glColorPointervINTEL(GLint size, GLenum type, const GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glTexCoordPointervINTEL glTexCoordPointervINTEL<c-o>:echoh HintHL<Bar>echo "void glTexCoordPointervINTEL(GLint size, GLenum type, const GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glPixelTransformParameteriEXT glPixelTransformParameteriEXT<c-o>:echoh HintHL<Bar>echo "void glPixelTransformParameteriEXT(GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPixelTransformParameterfEXT glPixelTransformParameterfEXT<c-o>:echoh HintHL<Bar>echo "void glPixelTransformParameterfEXT(GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPixelTransformParameterivEXT glPixelTransformParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glPixelTransformParameterivEXT(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glPixelTransformParameterfvEXT glPixelTransformParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glPixelTransformParameterfvEXT(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glSecondaryColor3bEXT glSecondaryColor3bEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3bEXT(GLbyte red, GLbyte green, GLbyte blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3bvEXT glSecondaryColor3bvEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3bvEXT(const GLbyte *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3dEXT glSecondaryColor3dEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3dEXT(GLdouble red, GLdouble green, GLdouble blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3dvEXT glSecondaryColor3dvEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3dvEXT(const GLdouble *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3fEXT glSecondaryColor3fEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3fEXT(GLfloat red, GLfloat green, GLfloat blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3fvEXT glSecondaryColor3fvEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3fvEXT(const GLfloat *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3iEXT glSecondaryColor3iEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3iEXT(GLint red, GLint green, GLint blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3ivEXT glSecondaryColor3ivEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3ivEXT(const GLint *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3sEXT glSecondaryColor3sEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3sEXT(GLshort red, GLshort green, GLshort blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3svEXT glSecondaryColor3svEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3svEXT(const GLshort *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3ubEXT glSecondaryColor3ubEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3ubEXT(GLubyte red, GLubyte green, GLubyte blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3ubvEXT glSecondaryColor3ubvEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3ubvEXT(const GLubyte *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3uiEXT glSecondaryColor3uiEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3uiEXT(GLuint red, GLuint green, GLuint blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3uivEXT glSecondaryColor3uivEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3uivEXT(const GLuint *v)"<Bar>echoh None<cr>
inorea glSecondaryColor3usEXT glSecondaryColor3usEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3usEXT(GLushort red, GLushort green, GLushort blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3usvEXT glSecondaryColor3usvEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3usvEXT(const GLushort *v)"<Bar>echoh None<cr>
inorea glSecondaryColorPointerEXT glSecondaryColorPointerEXT<c-o>:echoh HintHL<Bar>echo "void glSecondaryColorPointerEXT(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glTextureNormalEXT glTextureNormalEXT<c-o>:echoh HintHL<Bar>echo "void glTextureNormalEXT(GLenum mode)"<Bar>echoh None<cr>
inorea glMultiDrawArraysEXT glMultiDrawArraysEXT<c-o>:echoh HintHL<Bar>echo "void glMultiDrawArraysEXT(GLenum mode, const GLint *first, const GLsizei *count, GLsizei primcount)"<Bar>echoh None<cr>
inorea glMultiDrawElementsEXT glMultiDrawElementsEXT<c-o>:echoh HintHL<Bar>echo "void glMultiDrawElementsEXT(GLenum mode, const GLsizei *count, GLenum type, const GLvoid* *indices, GLsizei primcount)"<Bar>echoh None<cr>
inorea glFogCoordfEXT glFogCoordfEXT<c-o>:echoh HintHL<Bar>echo "void glFogCoordfEXT(GLfloat coord)"<Bar>echoh None<cr>
inorea glFogCoordfvEXT glFogCoordfvEXT<c-o>:echoh HintHL<Bar>echo "void glFogCoordfvEXT(const GLfloat *coord)"<Bar>echoh None<cr>
inorea glFogCoorddEXT glFogCoorddEXT<c-o>:echoh HintHL<Bar>echo "void glFogCoorddEXT(GLdouble coord)"<Bar>echoh None<cr>
inorea glFogCoorddvEXT glFogCoorddvEXT<c-o>:echoh HintHL<Bar>echo "void glFogCoorddvEXT(const GLdouble *coord)"<Bar>echoh None<cr>
inorea glFogCoordPointerEXT glFogCoordPointerEXT<c-o>:echoh HintHL<Bar>echo "void glFogCoordPointerEXT(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glTangent3bEXT glTangent3bEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3bEXT(GLbyte tx, GLbyte ty, GLbyte tz)"<Bar>echoh None<cr>
inorea glTangent3bvEXT glTangent3bvEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3bvEXT(const GLbyte *v)"<Bar>echoh None<cr>
inorea glTangent3dEXT glTangent3dEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3dEXT(GLdouble tx, GLdouble ty, GLdouble tz)"<Bar>echoh None<cr>
inorea glTangent3dvEXT glTangent3dvEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3dvEXT(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTangent3fEXT glTangent3fEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3fEXT(GLfloat tx, GLfloat ty, GLfloat tz)"<Bar>echoh None<cr>
inorea glTangent3fvEXT glTangent3fvEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3fvEXT(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTangent3iEXT glTangent3iEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3iEXT(GLint tx, GLint ty, GLint tz)"<Bar>echoh None<cr>
inorea glTangent3ivEXT glTangent3ivEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3ivEXT(const GLint *v)"<Bar>echoh None<cr>
inorea glTangent3sEXT glTangent3sEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3sEXT(GLshort tx, GLshort ty, GLshort tz)"<Bar>echoh None<cr>
inorea glTangent3svEXT glTangent3svEXT<c-o>:echoh HintHL<Bar>echo "void glTangent3svEXT(const GLshort *v)"<Bar>echoh None<cr>
inorea glBinormal3bEXT glBinormal3bEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3bEXT(GLbyte bx, GLbyte by, GLbyte bz)"<Bar>echoh None<cr>
inorea glBinormal3bvEXT glBinormal3bvEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3bvEXT(const GLbyte *v)"<Bar>echoh None<cr>
inorea glBinormal3dEXT glBinormal3dEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3dEXT(GLdouble bx, GLdouble by, GLdouble bz)"<Bar>echoh None<cr>
inorea glBinormal3dvEXT glBinormal3dvEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3dvEXT(const GLdouble *v)"<Bar>echoh None<cr>
inorea glBinormal3fEXT glBinormal3fEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3fEXT(GLfloat bx, GLfloat by, GLfloat bz)"<Bar>echoh None<cr>
inorea glBinormal3fvEXT glBinormal3fvEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3fvEXT(const GLfloat *v)"<Bar>echoh None<cr>
inorea glBinormal3iEXT glBinormal3iEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3iEXT(GLint bx, GLint by, GLint bz)"<Bar>echoh None<cr>
inorea glBinormal3ivEXT glBinormal3ivEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3ivEXT(const GLint *v)"<Bar>echoh None<cr>
inorea glBinormal3sEXT glBinormal3sEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3sEXT(GLshort bx, GLshort by, GLshort bz)"<Bar>echoh None<cr>
inorea glBinormal3svEXT glBinormal3svEXT<c-o>:echoh HintHL<Bar>echo "void glBinormal3svEXT(const GLshort *v)"<Bar>echoh None<cr>
inorea glTangentPointerEXT glTangentPointerEXT<c-o>:echoh HintHL<Bar>echo "void glTangentPointerEXT(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glBinormalPointerEXT glBinormalPointerEXT<c-o>:echoh HintHL<Bar>echo "void glBinormalPointerEXT(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glFinishTextureSUNX glFinishTextureSUNX<c-o>:echoh HintHL<Bar>echo "void glFinishTextureSUNX(void)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactorbSUN glGlobalAlphaFactorbSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactorbSUN(GLbyte factor)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactorsSUN glGlobalAlphaFactorsSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactorsSUN(GLshort factor)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactoriSUN glGlobalAlphaFactoriSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactoriSUN(GLint factor)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactorfSUN glGlobalAlphaFactorfSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactorfSUN(GLfloat factor)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactordSUN glGlobalAlphaFactordSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactordSUN(GLdouble factor)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactorubSUN glGlobalAlphaFactorubSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactorubSUN(GLubyte factor)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactorusSUN glGlobalAlphaFactorusSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactorusSUN(GLushort factor)"<Bar>echoh None<cr>
inorea glGlobalAlphaFactoruiSUN glGlobalAlphaFactoruiSUN<c-o>:echoh HintHL<Bar>echo "void glGlobalAlphaFactoruiSUN(GLuint factor)"<Bar>echoh None<cr>
inorea glReplacementCodeuiSUN glReplacementCodeuiSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiSUN(GLuint code)"<Bar>echoh None<cr>
inorea glReplacementCodeusSUN glReplacementCodeusSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeusSUN(GLushort code)"<Bar>echoh None<cr>
inorea glReplacementCodeubSUN glReplacementCodeubSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeubSUN(GLubyte code)"<Bar>echoh None<cr>
inorea glReplacementCodeuivSUN glReplacementCodeuivSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuivSUN(const GLuint *code)"<Bar>echoh None<cr>
inorea glReplacementCodeusvSUN glReplacementCodeusvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeusvSUN(const GLushort *code)"<Bar>echoh None<cr>
inorea glReplacementCodeubvSUN glReplacementCodeubvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeubvSUN(const GLubyte *code)"<Bar>echoh None<cr>
inorea glReplacementCodePointerSUN glReplacementCodePointerSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodePointerSUN(GLenum type, GLsizei stride, const GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glColor4ubVertex2fSUN glColor4ubVertex2fSUN<c-o>:echoh HintHL<Bar>echo "void glColor4ubVertex2fSUN(GLubyte r, GLubyte g, GLubyte b, GLubyte a, GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glColor4ubVertex2fvSUN glColor4ubVertex2fvSUN<c-o>:echoh HintHL<Bar>echo "void glColor4ubVertex2fvSUN(const GLubyte *c, const GLfloat *v)"<Bar>echoh None<cr>
inorea glColor4ubVertex3fSUN glColor4ubVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glColor4ubVertex3fSUN(GLubyte r, GLubyte g, GLubyte b, GLubyte a, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glColor4ubVertex3fvSUN glColor4ubVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glColor4ubVertex3fvSUN(const GLubyte *c, const GLfloat *v)"<Bar>echoh None<cr>
inorea glColor3fVertex3fSUN glColor3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glColor3fVertex3fSUN(GLfloat r, GLfloat g, GLfloat b, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glColor3fVertex3fvSUN glColor3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glColor3fVertex3fvSUN(const GLfloat *c, const GLfloat *v)"<Bar>echoh None<cr>
inorea glNormal3fVertex3fSUN glNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glNormal3fVertex3fSUN(GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glNormal3fVertex3fvSUN glNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glNormal3fVertex3fvSUN(const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glColor4fNormal3fVertex3fSUN glColor4fNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glColor4fNormal3fVertex3fSUN(GLfloat r, GLfloat g, GLfloat b, GLfloat a, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glColor4fNormal3fVertex3fvSUN glColor4fNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glColor4fNormal3fVertex3fvSUN(const GLfloat *c, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord2fVertex3fSUN glTexCoord2fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fVertex3fSUN(GLfloat s, GLfloat t, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glTexCoord2fVertex3fvSUN glTexCoord2fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fVertex3fvSUN(const GLfloat *tc, const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord4fVertex4fSUN glTexCoord4fVertex4fSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord4fVertex4fSUN(GLfloat s, GLfloat t, GLfloat p, GLfloat q, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glTexCoord4fVertex4fvSUN glTexCoord4fVertex4fvSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord4fVertex4fvSUN(const GLfloat *tc, const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord2fColor4ubVertex3fSUN glTexCoord2fColor4ubVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fColor4ubVertex3fSUN(GLfloat s, GLfloat t, GLubyte r, GLubyte g, GLubyte b, GLubyte a, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glTexCoord2fColor4ubVertex3fvSUN glTexCoord2fColor4ubVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fColor4ubVertex3fvSUN(const GLfloat *tc, const GLubyte *c, const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord2fColor3fVertex3fSUN glTexCoord2fColor3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fColor3fVertex3fSUN(GLfloat s, GLfloat t, GLfloat r, GLfloat g, GLfloat b, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glTexCoord2fColor3fVertex3fvSUN glTexCoord2fColor3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fColor3fVertex3fvSUN(const GLfloat *tc, const GLfloat *c, const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord2fNormal3fVertex3fSUN glTexCoord2fNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fNormal3fVertex3fSUN(GLfloat s, GLfloat t, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glTexCoord2fNormal3fVertex3fvSUN glTexCoord2fNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fNormal3fVertex3fvSUN(const GLfloat *tc, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord2fColor4fNormal3fVertex3fSUN glTexCoord2fColor4fNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fColor4fNormal3fVertex3fSUN(GLfloat s, GLfloat t, GLfloat r, GLfloat g, GLfloat b, GLfloat a, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glTexCoord2fColor4fNormal3fVertex3fvSUN glTexCoord2fColor4fNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fColor4fNormal3fVertex3fvSUN(const GLfloat *tc, const GLfloat *c, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord4fColor4fNormal3fVertex4fSUN glTexCoord4fColor4fNormal3fVertex4fSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord4fColor4fNormal3fVertex4fSUN(GLfloat s, GLfloat t, GLfloat p, GLfloat q, GLfloat r, GLfloat g, GLfloat b, GLfloat a, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glTexCoord4fColor4fNormal3fVertex4fvSUN glTexCoord4fColor4fNormal3fVertex4fvSUN<c-o>:echoh HintHL<Bar>echo "void glTexCoord4fColor4fNormal3fVertex4fvSUN(const GLfloat *tc, const GLfloat *c, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glReplacementCodeuiVertex3fSUN glReplacementCodeuiVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiVertex3fSUN(GLuint rc, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glReplacementCodeuiVertex3fvSUN glReplacementCodeuiVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiVertex3fvSUN(const GLuint *rc, const GLfloat *v)"<Bar>echoh None<cr>
inorea glReplacementCodeuiColor4ubVertex3fSUN glReplacementCodeuiColor4ubVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiColor4ubVertex3fSUN(GLuint rc, GLubyte r, GLubyte g, GLubyte b, GLubyte a, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glReplacementCodeuiColor4ubVertex3fvSUN glReplacementCodeuiColor4ubVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiColor4ubVertex3fvSUN(const GLuint *rc, const GLubyte *c, const GLfloat *v)"<Bar>echoh None<cr>
inorea glReplacementCodeuiColor3fVertex3fSUN glReplacementCodeuiColor3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiColor3fVertex3fSUN(GLuint rc, GLfloat r, GLfloat g, GLfloat b, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glReplacementCodeuiColor3fVertex3fvSUN glReplacementCodeuiColor3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiColor3fVertex3fvSUN(const GLuint *rc, const GLfloat *c, const GLfloat *v)"<Bar>echoh None<cr>
inorea glReplacementCodeuiNormal3fVertex3fSUN glReplacementCodeuiNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiNormal3fVertex3fSUN(GLuint rc, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glReplacementCodeuiNormal3fVertex3fvSUN glReplacementCodeuiNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiNormal3fVertex3fvSUN(const GLuint *rc, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glReplacementCodeuiColor4fNormal3fVertex3fSUN glReplacementCodeuiColor4fNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiColor4fNormal3fVertex3fSUN(GLuint rc, GLfloat r, GLfloat g, GLfloat b, GLfloat a, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glReplacementCodeuiColor4fNormal3fVertex3fvSUN glReplacementCodeuiColor4fNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiColor4fNormal3fVertex3fvSUN(const GLuint *rc, const GLfloat *c, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glReplacementCodeuiTexCoord2fVertex3fSUN glReplacementCodeuiTexCoord2fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiTexCoord2fVertex3fSUN(GLuint rc, GLfloat s, GLfloat t, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glReplacementCodeuiTexCoord2fVertex3fvSUN glReplacementCodeuiTexCoord2fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiTexCoord2fVertex3fvSUN(const GLuint *rc, const GLfloat *tc, const GLfloat *v)"<Bar>echoh None<cr>
inorea glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN(GLuint rc, GLfloat s, GLfloat t, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN(const GLuint *rc, const GLfloat *tc, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
"inorea glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN(GLuint rc, GLfloat s, GLfloat t, GLfloat r, GLfloat g, GLfloat b, GLfloat a, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
"inorea glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN<c-o>:echoh HintHL<Bar>echo "void glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN(const GLuint *rc, const GLfloat *tc, const GLfloat *c, const GLfloat *n, const GLfloat *v)"<Bar>echoh None<cr>
inorea glBlendFuncSeparateEXT glBlendFuncSeparateEXT<c-o>:echoh HintHL<Bar>echo "void glBlendFuncSeparateEXT(GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha)"<Bar>echoh None<cr>
inorea glBlendFuncSeparateINGR glBlendFuncSeparateINGR<c-o>:echoh HintHL<Bar>echo "void glBlendFuncSeparateINGR(GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha)"<Bar>echoh None<cr>
inorea glVertexWeightfEXT glVertexWeightfEXT<c-o>:echoh HintHL<Bar>echo "void glVertexWeightfEXT(GLfloat weight)"<Bar>echoh None<cr>
inorea glVertexWeightfvEXT glVertexWeightfvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexWeightfvEXT(const GLfloat *weight)"<Bar>echoh None<cr>
inorea glVertexWeightPointerEXT glVertexWeightPointerEXT<c-o>:echoh HintHL<Bar>echo "void glVertexWeightPointerEXT(GLsizei size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glFlushVertexArrayRangeNV glFlushVertexArrayRangeNV<c-o>:echoh HintHL<Bar>echo "void glFlushVertexArrayRangeNV(void)"<Bar>echoh None<cr>
inorea glVertexArrayRangeNV glVertexArrayRangeNV<c-o>:echoh HintHL<Bar>echo "void glVertexArrayRangeNV(GLsizei length, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glCombinerParameterfvNV glCombinerParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glCombinerParameterfvNV(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glCombinerParameterfNV glCombinerParameterfNV<c-o>:echoh HintHL<Bar>echo "void glCombinerParameterfNV(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glCombinerParameterivNV glCombinerParameterivNV<c-o>:echoh HintHL<Bar>echo "void glCombinerParameterivNV(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glCombinerParameteriNV glCombinerParameteriNV<c-o>:echoh HintHL<Bar>echo "void glCombinerParameteriNV(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glCombinerInputNV glCombinerInputNV<c-o>:echoh HintHL<Bar>echo "void glCombinerInputNV(GLenum stage, GLenum portion, GLenum variable, GLenum input, GLenum mapping, GLenum componentUsage)"<Bar>echoh None<cr>
inorea glCombinerOutputNV glCombinerOutputNV<c-o>:echoh HintHL<Bar>echo "void glCombinerOutputNV(GLenum stage, GLenum portion, GLenum abOutput, GLenum cdOutput, GLenum sumOutput, GLenum scale, GLenum bias, GLboolean abDotProduct, GLboolean cdDotProduct, GLboolean muxSum)"<Bar>echoh None<cr>
inorea glFinalCombinerInputNV glFinalCombinerInputNV<c-o>:echoh HintHL<Bar>echo "void glFinalCombinerInputNV(GLenum variable, GLenum input, GLenum mapping, GLenum componentUsage)"<Bar>echoh None<cr>
inorea glGetCombinerInputParameterfvNV glGetCombinerInputParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetCombinerInputParameterfvNV(GLenum stage, GLenum portion, GLenum variable, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetCombinerInputParameterivNV glGetCombinerInputParameterivNV<c-o>:echoh HintHL<Bar>echo "void glGetCombinerInputParameterivNV(GLenum stage, GLenum portion, GLenum variable, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetCombinerOutputParameterfvNV glGetCombinerOutputParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetCombinerOutputParameterfvNV(GLenum stage, GLenum portion, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetCombinerOutputParameterivNV glGetCombinerOutputParameterivNV<c-o>:echoh HintHL<Bar>echo "void glGetCombinerOutputParameterivNV(GLenum stage, GLenum portion, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetFinalCombinerInputParameterfvNV glGetFinalCombinerInputParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetFinalCombinerInputParameterfvNV(GLenum variable, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetFinalCombinerInputParameterivNV glGetFinalCombinerInputParameterivNV<c-o>:echoh HintHL<Bar>echo "void glGetFinalCombinerInputParameterivNV(GLenum variable, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glResizeBuffersMESA glResizeBuffersMESA<c-o>:echoh HintHL<Bar>echo "void glResizeBuffersMESA(void)"<Bar>echoh None<cr>
inorea glWindowPos2dMESA glWindowPos2dMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2dMESA(GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glWindowPos2dvMESA glWindowPos2dvMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2dvMESA(const GLdouble *v)"<Bar>echoh None<cr>
inorea glWindowPos2fMESA glWindowPos2fMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2fMESA(GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glWindowPos2fvMESA glWindowPos2fvMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2fvMESA(const GLfloat *v)"<Bar>echoh None<cr>
inorea glWindowPos2iMESA glWindowPos2iMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2iMESA(GLint x, GLint y)"<Bar>echoh None<cr>
inorea glWindowPos2ivMESA glWindowPos2ivMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2ivMESA(const GLint *v)"<Bar>echoh None<cr>
inorea glWindowPos2sMESA glWindowPos2sMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2sMESA(GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glWindowPos2svMESA glWindowPos2svMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos2svMESA(const GLshort *v)"<Bar>echoh None<cr>
inorea glWindowPos3dMESA glWindowPos3dMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3dMESA(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glWindowPos3dvMESA glWindowPos3dvMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3dvMESA(const GLdouble *v)"<Bar>echoh None<cr>
inorea glWindowPos3fMESA glWindowPos3fMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3fMESA(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glWindowPos3fvMESA glWindowPos3fvMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3fvMESA(const GLfloat *v)"<Bar>echoh None<cr>
inorea glWindowPos3iMESA glWindowPos3iMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3iMESA(GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glWindowPos3ivMESA glWindowPos3ivMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3ivMESA(const GLint *v)"<Bar>echoh None<cr>
inorea glWindowPos3sMESA glWindowPos3sMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3sMESA(GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glWindowPos3svMESA glWindowPos3svMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos3svMESA(const GLshort *v)"<Bar>echoh None<cr>
inorea glWindowPos4dMESA glWindowPos4dMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4dMESA(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glWindowPos4dvMESA glWindowPos4dvMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4dvMESA(const GLdouble *v)"<Bar>echoh None<cr>
inorea glWindowPos4fMESA glWindowPos4fMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4fMESA(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glWindowPos4fvMESA glWindowPos4fvMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4fvMESA(const GLfloat *v)"<Bar>echoh None<cr>
inorea glWindowPos4iMESA glWindowPos4iMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4iMESA(GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glWindowPos4ivMESA glWindowPos4ivMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4ivMESA(const GLint *v)"<Bar>echoh None<cr>
inorea glWindowPos4sMESA glWindowPos4sMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4sMESA(GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glWindowPos4svMESA glWindowPos4svMESA<c-o>:echoh HintHL<Bar>echo "void glWindowPos4svMESA(const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiModeDrawArraysIBM glMultiModeDrawArraysIBM<c-o>:echoh HintHL<Bar>echo "void glMultiModeDrawArraysIBM(const GLenum *mode, const GLint *first, const GLsizei *count, GLsizei primcount, GLint modestride)"<Bar>echoh None<cr>
inorea glMultiModeDrawElementsIBM glMultiModeDrawElementsIBM<c-o>:echoh HintHL<Bar>echo "void glMultiModeDrawElementsIBM(const GLenum *mode, const GLsizei *count, GLenum type, const GLvoid* const *indices, GLsizei primcount, GLint modestride)"<Bar>echoh None<cr>
inorea glColorPointerListIBM glColorPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glColorPointerListIBM(GLint size, GLenum type, GLint stride, const GLvoid* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glSecondaryColorPointerListIBM glSecondaryColorPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glSecondaryColorPointerListIBM(GLint size, GLenum type, GLint stride, const GLvoid* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glEdgeFlagPointerListIBM glEdgeFlagPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glEdgeFlagPointerListIBM(GLint stride, const GLboolean* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glFogCoordPointerListIBM glFogCoordPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glFogCoordPointerListIBM(GLenum type, GLint stride, const GLvoid* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glIndexPointerListIBM glIndexPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glIndexPointerListIBM(GLenum type, GLint stride, const GLvoid* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glNormalPointerListIBM glNormalPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glNormalPointerListIBM(GLenum type, GLint stride, const GLvoid* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glTexCoordPointerListIBM glTexCoordPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glTexCoordPointerListIBM(GLint size, GLenum type, GLint stride, const GLvoid* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glVertexPointerListIBM glVertexPointerListIBM<c-o>:echoh HintHL<Bar>echo "void glVertexPointerListIBM(GLint size, GLenum type, GLint stride, const GLvoid* *pointer, GLint ptrstride)"<Bar>echoh None<cr>
inorea glTbufferMask3DFX glTbufferMask3DFX<c-o>:echoh HintHL<Bar>echo "void glTbufferMask3DFX(GLuint mask)"<Bar>echoh None<cr>
inorea glSampleMaskEXT glSampleMaskEXT<c-o>:echoh HintHL<Bar>echo "void glSampleMaskEXT(GLclampf value, GLboolean invert)"<Bar>echoh None<cr>
inorea glSamplePatternEXT glSamplePatternEXT<c-o>:echoh HintHL<Bar>echo "void glSamplePatternEXT(GLenum pattern)"<Bar>echoh None<cr>
inorea glTextureColorMaskSGIS glTextureColorMaskSGIS<c-o>:echoh HintHL<Bar>echo "void glTextureColorMaskSGIS(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)"<Bar>echoh None<cr>
inorea glIglooInterfaceSGIX glIglooInterfaceSGIX<c-o>:echoh HintHL<Bar>echo "void glIglooInterfaceSGIX(GLenum pname, const GLvoid *params)"<Bar>echoh None<cr>
inorea glDeleteFencesNV glDeleteFencesNV<c-o>:echoh HintHL<Bar>echo "void glDeleteFencesNV(GLsizei n, const GLuint *fences)"<Bar>echoh None<cr>
inorea glGenFencesNV glGenFencesNV<c-o>:echoh HintHL<Bar>echo "void glGenFencesNV(GLsizei n, GLuint *fences)"<Bar>echoh None<cr>
inorea glIsFenceNV glIsFenceNV<c-o>:echoh HintHL<Bar>echo "GLboolean glIsFenceNV(GLuint fence)"<Bar>echoh None<cr>
inorea glTestFenceNV glTestFenceNV<c-o>:echoh HintHL<Bar>echo "GLboolean glTestFenceNV(GLuint fence)"<Bar>echoh None<cr>
inorea glGetFenceivNV glGetFenceivNV<c-o>:echoh HintHL<Bar>echo "void glGetFenceivNV(GLuint fence, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glFinishFenceNV glFinishFenceNV<c-o>:echoh HintHL<Bar>echo "void glFinishFenceNV(GLuint fence)"<Bar>echoh None<cr>
inorea glSetFenceNV glSetFenceNV<c-o>:echoh HintHL<Bar>echo "void glSetFenceNV(GLuint fence, GLenum condition)"<Bar>echoh None<cr>
inorea glMapControlPointsNV glMapControlPointsNV<c-o>:echoh HintHL<Bar>echo "void glMapControlPointsNV(GLenum target, GLuint index, GLenum type, GLsizei ustride, GLsizei vstride, GLint uorder, GLint vorder, GLboolean packed, const GLvoid *points)"<Bar>echoh None<cr>
inorea glMapParameterivNV glMapParameterivNV<c-o>:echoh HintHL<Bar>echo "void glMapParameterivNV(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glMapParameterfvNV glMapParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glMapParameterfvNV(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMapControlPointsNV glGetMapControlPointsNV<c-o>:echoh HintHL<Bar>echo "void glGetMapControlPointsNV(GLenum target, GLuint index, GLenum type, GLsizei ustride, GLsizei vstride, GLboolean packed, GLvoid *points)"<Bar>echoh None<cr>
inorea glGetMapParameterivNV glGetMapParameterivNV<c-o>:echoh HintHL<Bar>echo "void glGetMapParameterivNV(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMapParameterfvNV glGetMapParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetMapParameterfvNV(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMapAttribParameterivNV glGetMapAttribParameterivNV<c-o>:echoh HintHL<Bar>echo "void glGetMapAttribParameterivNV(GLenum target, GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMapAttribParameterfvNV glGetMapAttribParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetMapAttribParameterfvNV(GLenum target, GLuint index, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glEvalMapsNV glEvalMapsNV<c-o>:echoh HintHL<Bar>echo "void glEvalMapsNV(GLenum target, GLenum mode)"<Bar>echoh None<cr>
inorea glCombinerStageParameterfvNV glCombinerStageParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glCombinerStageParameterfvNV(GLenum stage, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetCombinerStageParameterfvNV glGetCombinerStageParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetCombinerStageParameterfvNV(GLenum stage, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glAreProgramsResidentNV glAreProgramsResidentNV<c-o>:echoh HintHL<Bar>echo "GLboolean glAreProgramsResidentNV(GLsizei n, const GLuint *programs, GLboolean *residences)"<Bar>echoh None<cr>
inorea glBindProgramNV glBindProgramNV<c-o>:echoh HintHL<Bar>echo "void glBindProgramNV(GLenum target, GLuint id)"<Bar>echoh None<cr>
inorea glDeleteProgramsNV glDeleteProgramsNV<c-o>:echoh HintHL<Bar>echo "void glDeleteProgramsNV(GLsizei n, const GLuint *programs)"<Bar>echoh None<cr>
inorea glExecuteProgramNV glExecuteProgramNV<c-o>:echoh HintHL<Bar>echo "void glExecuteProgramNV(GLenum target, GLuint id, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGenProgramsNV glGenProgramsNV<c-o>:echoh HintHL<Bar>echo "void glGenProgramsNV(GLsizei n, GLuint *programs)"<Bar>echoh None<cr>
inorea glGetProgramParameterdvNV glGetProgramParameterdvNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramParameterdvNV(GLenum target, GLuint index, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetProgramParameterfvNV glGetProgramParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramParameterfvNV(GLenum target, GLuint index, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetProgramivNV glGetProgramivNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramivNV(GLuint id, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetProgramStringNV glGetProgramStringNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramStringNV(GLuint id, GLenum pname, GLubyte *program)"<Bar>echoh None<cr>
inorea glGetTrackMatrixivNV glGetTrackMatrixivNV<c-o>:echoh HintHL<Bar>echo "void glGetTrackMatrixivNV(GLenum target, GLuint address, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribdvNV glGetVertexAttribdvNV<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribdvNV(GLuint index, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribfvNV glGetVertexAttribfvNV<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribfvNV(GLuint index, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribivNV glGetVertexAttribivNV<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribivNV(GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribPointervNV glGetVertexAttribPointervNV<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribPointervNV(GLuint index, GLenum pname, GLvoid* *pointer)"<Bar>echoh None<cr>
inorea glIsProgramNV glIsProgramNV<c-o>:echoh HintHL<Bar>echo "GLboolean glIsProgramNV(GLuint id)"<Bar>echoh None<cr>
inorea glLoadProgramNV glLoadProgramNV<c-o>:echoh HintHL<Bar>echo "void glLoadProgramNV(GLenum target, GLuint id, GLsizei len, const GLubyte *program)"<Bar>echoh None<cr>
inorea glProgramParameter4dNV glProgramParameter4dNV<c-o>:echoh HintHL<Bar>echo "void glProgramParameter4dNV(GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glProgramParameter4dvNV glProgramParameter4dvNV<c-o>:echoh HintHL<Bar>echo "void glProgramParameter4dvNV(GLenum target, GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glProgramParameter4fNV glProgramParameter4fNV<c-o>:echoh HintHL<Bar>echo "void glProgramParameter4fNV(GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glProgramParameter4fvNV glProgramParameter4fvNV<c-o>:echoh HintHL<Bar>echo "void glProgramParameter4fvNV(GLenum target, GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glProgramParameters4dvNV glProgramParameters4dvNV<c-o>:echoh HintHL<Bar>echo "void glProgramParameters4dvNV(GLenum target, GLuint index, GLsizei count, const GLdouble *v)"<Bar>echoh None<cr>
inorea glProgramParameters4fvNV glProgramParameters4fvNV<c-o>:echoh HintHL<Bar>echo "void glProgramParameters4fvNV(GLenum target, GLuint index, GLsizei count, const GLfloat *v)"<Bar>echoh None<cr>
inorea glRequestResidentProgramsNV glRequestResidentProgramsNV<c-o>:echoh HintHL<Bar>echo "void glRequestResidentProgramsNV(GLsizei n, const GLuint *programs)"<Bar>echoh None<cr>
inorea glTrackMatrixNV glTrackMatrixNV<c-o>:echoh HintHL<Bar>echo "void glTrackMatrixNV(GLenum target, GLuint address, GLenum matrix, GLenum transform)"<Bar>echoh None<cr>
inorea glVertexAttribPointerNV glVertexAttribPointerNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribPointerNV(GLuint index, GLint fsize, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glVertexAttrib1dNV glVertexAttrib1dNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1dNV(GLuint index, GLdouble x)"<Bar>echoh None<cr>
inorea glVertexAttrib1dvNV glVertexAttrib1dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1dvNV(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib1fNV glVertexAttrib1fNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1fNV(GLuint index, GLfloat x)"<Bar>echoh None<cr>
inorea glVertexAttrib1fvNV glVertexAttrib1fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1fvNV(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib1sNV glVertexAttrib1sNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1sNV(GLuint index, GLshort x)"<Bar>echoh None<cr>
inorea glVertexAttrib1svNV glVertexAttrib1svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1svNV(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2dNV glVertexAttrib2dNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2dNV(GLuint index, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertexAttrib2dvNV glVertexAttrib2dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2dvNV(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2fNV glVertexAttrib2fNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2fNV(GLuint index, GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glVertexAttrib2fvNV glVertexAttrib2fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2fvNV(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2sNV glVertexAttrib2sNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2sNV(GLuint index, GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glVertexAttrib2svNV glVertexAttrib2svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2svNV(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3dNV glVertexAttrib3dNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3dNV(GLuint index, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertexAttrib3dvNV glVertexAttrib3dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3dvNV(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3fNV glVertexAttrib3fNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3fNV(GLuint index, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glVertexAttrib3fvNV glVertexAttrib3fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3fvNV(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3sNV glVertexAttrib3sNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3sNV(GLuint index, GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glVertexAttrib3svNV glVertexAttrib3svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3svNV(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4dNV glVertexAttrib4dNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4dNV(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertexAttrib4dvNV glVertexAttrib4dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4dvNV(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4fNV glVertexAttrib4fNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4fNV(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glVertexAttrib4fvNV glVertexAttrib4fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4fvNV(GLuint index, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4sNV glVertexAttrib4sNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4sNV(GLuint index, GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glVertexAttrib4svNV glVertexAttrib4svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4svNV(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4ubNV glVertexAttrib4ubNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4ubNV(GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w)"<Bar>echoh None<cr>
inorea glVertexAttrib4ubvNV glVertexAttrib4ubvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4ubvNV(GLuint index, const GLubyte *v)"<Bar>echoh None<cr>
inorea glVertexAttribs1dvNV glVertexAttribs1dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs1dvNV(GLuint index, GLsizei count, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribs1fvNV glVertexAttribs1fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs1fvNV(GLuint index, GLsizei count, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttribs1svNV glVertexAttribs1svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs1svNV(GLuint index, GLsizei count, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttribs2dvNV glVertexAttribs2dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs2dvNV(GLuint index, GLsizei count, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribs2fvNV glVertexAttribs2fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs2fvNV(GLuint index, GLsizei count, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttribs2svNV glVertexAttribs2svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs2svNV(GLuint index, GLsizei count, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttribs3dvNV glVertexAttribs3dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs3dvNV(GLuint index, GLsizei count, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribs3fvNV glVertexAttribs3fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs3fvNV(GLuint index, GLsizei count, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttribs3svNV glVertexAttribs3svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs3svNV(GLuint index, GLsizei count, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttribs4dvNV glVertexAttribs4dvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs4dvNV(GLuint index, GLsizei count, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribs4fvNV glVertexAttribs4fvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs4fvNV(GLuint index, GLsizei count, const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertexAttribs4svNV glVertexAttribs4svNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs4svNV(GLuint index, GLsizei count, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttribs4ubvNV glVertexAttribs4ubvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs4ubvNV(GLuint index, GLsizei count, const GLubyte *v)"<Bar>echoh None<cr>
inorea glTexBumpParameterivATI glTexBumpParameterivATI<c-o>:echoh HintHL<Bar>echo "void glTexBumpParameterivATI(GLenum pname, const GLint *param)"<Bar>echoh None<cr>
inorea glTexBumpParameterfvATI glTexBumpParameterfvATI<c-o>:echoh HintHL<Bar>echo "void glTexBumpParameterfvATI(GLenum pname, const GLfloat *param)"<Bar>echoh None<cr>
inorea glGetTexBumpParameterivATI glGetTexBumpParameterivATI<c-o>:echoh HintHL<Bar>echo "void glGetTexBumpParameterivATI(GLenum pname, GLint *param)"<Bar>echoh None<cr>
inorea glGetTexBumpParameterfvATI glGetTexBumpParameterfvATI<c-o>:echoh HintHL<Bar>echo "void glGetTexBumpParameterfvATI(GLenum pname, GLfloat *param)"<Bar>echoh None<cr>
inorea glGenFragmentShadersATI glGenFragmentShadersATI<c-o>:echoh HintHL<Bar>echo "GLuint glGenFragmentShadersATI(GLuint range)"<Bar>echoh None<cr>
inorea glBindFragmentShaderATI glBindFragmentShaderATI<c-o>:echoh HintHL<Bar>echo "void glBindFragmentShaderATI(GLuint id)"<Bar>echoh None<cr>
inorea glDeleteFragmentShaderATI glDeleteFragmentShaderATI<c-o>:echoh HintHL<Bar>echo "void glDeleteFragmentShaderATI(GLuint id)"<Bar>echoh None<cr>
inorea glBeginFragmentShaderATI glBeginFragmentShaderATI<c-o>:echoh HintHL<Bar>echo "void glBeginFragmentShaderATI(void)"<Bar>echoh None<cr>
inorea glEndFragmentShaderATI glEndFragmentShaderATI<c-o>:echoh HintHL<Bar>echo "void glEndFragmentShaderATI(void)"<Bar>echoh None<cr>
inorea glPassTexCoordATI glPassTexCoordATI<c-o>:echoh HintHL<Bar>echo "void glPassTexCoordATI(GLuint dst, GLuint coord, GLenum swizzle)"<Bar>echoh None<cr>
inorea glSampleMapATI glSampleMapATI<c-o>:echoh HintHL<Bar>echo "void glSampleMapATI(GLuint dst, GLuint interp, GLenum swizzle)"<Bar>echoh None<cr>
inorea glColorFragmentOp1ATI glColorFragmentOp1ATI<c-o>:echoh HintHL<Bar>echo "void glColorFragmentOp1ATI(GLenum op, GLuint dst, GLuint dstMask, GLuint dstMod, GLuint arg1, GLuint arg1Rep, GLuint arg1Mod)"<Bar>echoh None<cr>
inorea glColorFragmentOp2ATI glColorFragmentOp2ATI<c-o>:echoh HintHL<Bar>echo "void glColorFragmentOp2ATI(GLenum op, GLuint dst, GLuint dstMask, GLuint dstMod, GLuint arg1, GLuint arg1Rep, GLuint arg1Mod, GLuint arg2, GLuint arg2Rep, GLuint arg2Mod)"<Bar>echoh None<cr>
inorea glColorFragmentOp3ATI glColorFragmentOp3ATI<c-o>:echoh HintHL<Bar>echo "void glColorFragmentOp3ATI(GLenum op, GLuint dst, GLuint dstMask, GLuint dstMod, GLuint arg1, GLuint arg1Rep, GLuint arg1Mod, GLuint arg2, GLuint arg2Rep, GLuint arg2Mod, GLuint arg3, GLuint arg3Rep, GLuint arg3Mod)"<Bar>echoh None<cr>
inorea glAlphaFragmentOp1ATI glAlphaFragmentOp1ATI<c-o>:echoh HintHL<Bar>echo "void glAlphaFragmentOp1ATI(GLenum op, GLuint dst, GLuint dstMod, GLuint arg1, GLuint arg1Rep, GLuint arg1Mod)"<Bar>echoh None<cr>
inorea glAlphaFragmentOp2ATI glAlphaFragmentOp2ATI<c-o>:echoh HintHL<Bar>echo "void glAlphaFragmentOp2ATI(GLenum op, GLuint dst, GLuint dstMod, GLuint arg1, GLuint arg1Rep, GLuint arg1Mod, GLuint arg2, GLuint arg2Rep, GLuint arg2Mod)"<Bar>echoh None<cr>
inorea glAlphaFragmentOp3ATI glAlphaFragmentOp3ATI<c-o>:echoh HintHL<Bar>echo "void glAlphaFragmentOp3ATI(GLenum op, GLuint dst, GLuint dstMod, GLuint arg1, GLuint arg1Rep, GLuint arg1Mod, GLuint arg2, GLuint arg2Rep, GLuint arg2Mod, GLuint arg3, GLuint arg3Rep, GLuint arg3Mod)"<Bar>echoh None<cr>
inorea glSetFragmentShaderConstantATI glSetFragmentShaderConstantATI<c-o>:echoh HintHL<Bar>echo "void glSetFragmentShaderConstantATI(GLuint dst, const GLfloat *value)"<Bar>echoh None<cr>
inorea glPNTrianglesiATI glPNTrianglesiATI<c-o>:echoh HintHL<Bar>echo "void glPNTrianglesiATI(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPNTrianglesfATI glPNTrianglesfATI<c-o>:echoh HintHL<Bar>echo "void glPNTrianglesfATI(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glNewObjectBufferATI glNewObjectBufferATI<c-o>:echoh HintHL<Bar>echo "GLuint glNewObjectBufferATI(GLsizei size, const GLvoid *pointer, GLenum usage)"<Bar>echoh None<cr>
inorea glIsObjectBufferATI glIsObjectBufferATI<c-o>:echoh HintHL<Bar>echo "GLboolean glIsObjectBufferATI(GLuint buffer)"<Bar>echoh None<cr>
inorea glUpdateObjectBufferATI glUpdateObjectBufferATI<c-o>:echoh HintHL<Bar>echo "void glUpdateObjectBufferATI(GLuint buffer, GLuint offset, GLsizei size, const GLvoid *pointer, GLenum preserve)"<Bar>echoh None<cr>
inorea glGetObjectBufferfvATI glGetObjectBufferfvATI<c-o>:echoh HintHL<Bar>echo "void glGetObjectBufferfvATI(GLuint buffer, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetObjectBufferivATI glGetObjectBufferivATI<c-o>:echoh HintHL<Bar>echo "void glGetObjectBufferivATI(GLuint buffer, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glFreeObjectBufferATI glFreeObjectBufferATI<c-o>:echoh HintHL<Bar>echo "void glFreeObjectBufferATI(GLuint buffer)"<Bar>echoh None<cr>
inorea glArrayObjectATI glArrayObjectATI<c-o>:echoh HintHL<Bar>echo "void glArrayObjectATI(GLenum array, GLint size, GLenum type, GLsizei stride, GLuint buffer, GLuint offset)"<Bar>echoh None<cr>
inorea glGetArrayObjectfvATI glGetArrayObjectfvATI<c-o>:echoh HintHL<Bar>echo "void glGetArrayObjectfvATI(GLenum array, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetArrayObjectivATI glGetArrayObjectivATI<c-o>:echoh HintHL<Bar>echo "void glGetArrayObjectivATI(GLenum array, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glVariantArrayObjectATI glVariantArrayObjectATI<c-o>:echoh HintHL<Bar>echo "void glVariantArrayObjectATI(GLuint id, GLenum type, GLsizei stride, GLuint buffer, GLuint offset)"<Bar>echoh None<cr>
inorea glGetVariantArrayObjectfvATI glGetVariantArrayObjectfvATI<c-o>:echoh HintHL<Bar>echo "void glGetVariantArrayObjectfvATI(GLuint id, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetVariantArrayObjectivATI glGetVariantArrayObjectivATI<c-o>:echoh HintHL<Bar>echo "void glGetVariantArrayObjectivATI(GLuint id, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glBeginVertexShaderEXT glBeginVertexShaderEXT<c-o>:echoh HintHL<Bar>echo "void glBeginVertexShaderEXT(void)"<Bar>echoh None<cr>
inorea glEndVertexShaderEXT glEndVertexShaderEXT<c-o>:echoh HintHL<Bar>echo "void glEndVertexShaderEXT(void)"<Bar>echoh None<cr>
inorea glBindVertexShaderEXT glBindVertexShaderEXT<c-o>:echoh HintHL<Bar>echo "void glBindVertexShaderEXT(GLuint id)"<Bar>echoh None<cr>
inorea glGenVertexShadersEXT glGenVertexShadersEXT<c-o>:echoh HintHL<Bar>echo "GLuint glGenVertexShadersEXT(GLuint range)"<Bar>echoh None<cr>
inorea glDeleteVertexShaderEXT glDeleteVertexShaderEXT<c-o>:echoh HintHL<Bar>echo "void glDeleteVertexShaderEXT(GLuint id)"<Bar>echoh None<cr>
inorea glShaderOp1EXT glShaderOp1EXT<c-o>:echoh HintHL<Bar>echo "void glShaderOp1EXT(GLenum op, GLuint res, GLuint arg1)"<Bar>echoh None<cr>
inorea glShaderOp2EXT glShaderOp2EXT<c-o>:echoh HintHL<Bar>echo "void glShaderOp2EXT(GLenum op, GLuint res, GLuint arg1, GLuint arg2)"<Bar>echoh None<cr>
inorea glShaderOp3EXT glShaderOp3EXT<c-o>:echoh HintHL<Bar>echo "void glShaderOp3EXT(GLenum op, GLuint res, GLuint arg1, GLuint arg2, GLuint arg3)"<Bar>echoh None<cr>
inorea glSwizzleEXT glSwizzleEXT<c-o>:echoh HintHL<Bar>echo "void glSwizzleEXT(GLuint res, GLuint in, GLenum outX, GLenum outY, GLenum outZ, GLenum outW)"<Bar>echoh None<cr>
inorea glWriteMaskEXT glWriteMaskEXT<c-o>:echoh HintHL<Bar>echo "void glWriteMaskEXT(GLuint res, GLuint in, GLenum outX, GLenum outY, GLenum outZ, GLenum outW)"<Bar>echoh None<cr>
inorea glInsertComponentEXT glInsertComponentEXT<c-o>:echoh HintHL<Bar>echo "void glInsertComponentEXT(GLuint res, GLuint src, GLuint num)"<Bar>echoh None<cr>
inorea glExtractComponentEXT glExtractComponentEXT<c-o>:echoh HintHL<Bar>echo "void glExtractComponentEXT(GLuint res, GLuint src, GLuint num)"<Bar>echoh None<cr>
inorea glGenSymbolsEXT glGenSymbolsEXT<c-o>:echoh HintHL<Bar>echo "GLuint glGenSymbolsEXT(GLenum datatype, GLenum storagetype, GLenum range, GLuint components)"<Bar>echoh None<cr>
inorea glSetInvariantEXT glSetInvariantEXT<c-o>:echoh HintHL<Bar>echo "void glSetInvariantEXT(GLuint id, GLenum type, const GLvoid *addr)"<Bar>echoh None<cr>
inorea glSetLocalConstantEXT glSetLocalConstantEXT<c-o>:echoh HintHL<Bar>echo "void glSetLocalConstantEXT(GLuint id, GLenum type, const GLvoid *addr)"<Bar>echoh None<cr>
inorea glVariantbvEXT glVariantbvEXT<c-o>:echoh HintHL<Bar>echo "void glVariantbvEXT(GLuint id, const GLbyte *addr)"<Bar>echoh None<cr>
inorea glVariantsvEXT glVariantsvEXT<c-o>:echoh HintHL<Bar>echo "void glVariantsvEXT(GLuint id, const GLshort *addr)"<Bar>echoh None<cr>
inorea glVariantivEXT glVariantivEXT<c-o>:echoh HintHL<Bar>echo "void glVariantivEXT(GLuint id, const GLint *addr)"<Bar>echoh None<cr>
inorea glVariantfvEXT glVariantfvEXT<c-o>:echoh HintHL<Bar>echo "void glVariantfvEXT(GLuint id, const GLfloat *addr)"<Bar>echoh None<cr>
inorea glVariantdvEXT glVariantdvEXT<c-o>:echoh HintHL<Bar>echo "void glVariantdvEXT(GLuint id, const GLdouble *addr)"<Bar>echoh None<cr>
inorea glVariantubvEXT glVariantubvEXT<c-o>:echoh HintHL<Bar>echo "void glVariantubvEXT(GLuint id, const GLubyte *addr)"<Bar>echoh None<cr>
inorea glVariantusvEXT glVariantusvEXT<c-o>:echoh HintHL<Bar>echo "void glVariantusvEXT(GLuint id, const GLushort *addr)"<Bar>echoh None<cr>
inorea glVariantuivEXT glVariantuivEXT<c-o>:echoh HintHL<Bar>echo "void glVariantuivEXT(GLuint id, const GLuint *addr)"<Bar>echoh None<cr>
inorea glVariantPointerEXT glVariantPointerEXT<c-o>:echoh HintHL<Bar>echo "void glVariantPointerEXT(GLuint id, GLenum type, GLuint stride, const GLvoid *addr)"<Bar>echoh None<cr>
inorea glEnableVariantClientStateEXT glEnableVariantClientStateEXT<c-o>:echoh HintHL<Bar>echo "void glEnableVariantClientStateEXT(GLuint id)"<Bar>echoh None<cr>
inorea glDisableVariantClientStateEXT glDisableVariantClientStateEXT<c-o>:echoh HintHL<Bar>echo "void glDisableVariantClientStateEXT(GLuint id)"<Bar>echoh None<cr>
inorea glBindLightParameterEXT glBindLightParameterEXT<c-o>:echoh HintHL<Bar>echo "GLuint glBindLightParameterEXT(GLenum light, GLenum value)"<Bar>echoh None<cr>
inorea glBindMaterialParameterEXT glBindMaterialParameterEXT<c-o>:echoh HintHL<Bar>echo "GLuint glBindMaterialParameterEXT(GLenum face, GLenum value)"<Bar>echoh None<cr>
inorea glBindTexGenParameterEXT glBindTexGenParameterEXT<c-o>:echoh HintHL<Bar>echo "GLuint glBindTexGenParameterEXT(GLenum unit, GLenum coord, GLenum value)"<Bar>echoh None<cr>
inorea glBindTextureUnitParameterEXT glBindTextureUnitParameterEXT<c-o>:echoh HintHL<Bar>echo "GLuint glBindTextureUnitParameterEXT(GLenum unit, GLenum value)"<Bar>echoh None<cr>
inorea glBindParameterEXT glBindParameterEXT<c-o>:echoh HintHL<Bar>echo "GLuint glBindParameterEXT(GLenum value)"<Bar>echoh None<cr>
inorea glIsVariantEnabledEXT glIsVariantEnabledEXT<c-o>:echoh HintHL<Bar>echo "GLboolean glIsVariantEnabledEXT(GLuint id, GLenum cap)"<Bar>echoh None<cr>
inorea glGetVariantBooleanvEXT glGetVariantBooleanvEXT<c-o>:echoh HintHL<Bar>echo "void glGetVariantBooleanvEXT(GLuint id, GLenum value, GLboolean *data)"<Bar>echoh None<cr>
inorea glGetVariantIntegervEXT glGetVariantIntegervEXT<c-o>:echoh HintHL<Bar>echo "void glGetVariantIntegervEXT(GLuint id, GLenum value, GLint *data)"<Bar>echoh None<cr>
inorea glGetVariantFloatvEXT glGetVariantFloatvEXT<c-o>:echoh HintHL<Bar>echo "void glGetVariantFloatvEXT(GLuint id, GLenum value, GLfloat *data)"<Bar>echoh None<cr>
inorea glGetVariantPointervEXT glGetVariantPointervEXT<c-o>:echoh HintHL<Bar>echo "void glGetVariantPointervEXT(GLuint id, GLenum value, GLvoid* *data)"<Bar>echoh None<cr>
inorea glGetInvariantBooleanvEXT glGetInvariantBooleanvEXT<c-o>:echoh HintHL<Bar>echo "void glGetInvariantBooleanvEXT(GLuint id, GLenum value, GLboolean *data)"<Bar>echoh None<cr>
inorea glGetInvariantIntegervEXT glGetInvariantIntegervEXT<c-o>:echoh HintHL<Bar>echo "void glGetInvariantIntegervEXT(GLuint id, GLenum value, GLint *data)"<Bar>echoh None<cr>
inorea glGetInvariantFloatvEXT glGetInvariantFloatvEXT<c-o>:echoh HintHL<Bar>echo "void glGetInvariantFloatvEXT(GLuint id, GLenum value, GLfloat *data)"<Bar>echoh None<cr>
inorea glGetLocalConstantBooleanvEXT glGetLocalConstantBooleanvEXT<c-o>:echoh HintHL<Bar>echo "void glGetLocalConstantBooleanvEXT(GLuint id, GLenum value, GLboolean *data)"<Bar>echoh None<cr>
inorea glGetLocalConstantIntegervEXT glGetLocalConstantIntegervEXT<c-o>:echoh HintHL<Bar>echo "void glGetLocalConstantIntegervEXT(GLuint id, GLenum value, GLint *data)"<Bar>echoh None<cr>
inorea glGetLocalConstantFloatvEXT glGetLocalConstantFloatvEXT<c-o>:echoh HintHL<Bar>echo "void glGetLocalConstantFloatvEXT(GLuint id, GLenum value, GLfloat *data)"<Bar>echoh None<cr>
inorea glVertexStream1sATI glVertexStream1sATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1sATI(GLenum stream, GLshort x)"<Bar>echoh None<cr>
inorea glVertexStream1svATI glVertexStream1svATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1svATI(GLenum stream, const GLshort *coords)"<Bar>echoh None<cr>
inorea glVertexStream1iATI glVertexStream1iATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1iATI(GLenum stream, GLint x)"<Bar>echoh None<cr>
inorea glVertexStream1ivATI glVertexStream1ivATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1ivATI(GLenum stream, const GLint *coords)"<Bar>echoh None<cr>
inorea glVertexStream1fATI glVertexStream1fATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1fATI(GLenum stream, GLfloat x)"<Bar>echoh None<cr>
inorea glVertexStream1fvATI glVertexStream1fvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1fvATI(GLenum stream, const GLfloat *coords)"<Bar>echoh None<cr>
inorea glVertexStream1dATI glVertexStream1dATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1dATI(GLenum stream, GLdouble x)"<Bar>echoh None<cr>
inorea glVertexStream1dvATI glVertexStream1dvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream1dvATI(GLenum stream, const GLdouble *coords)"<Bar>echoh None<cr>
inorea glVertexStream2sATI glVertexStream2sATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2sATI(GLenum stream, GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glVertexStream2svATI glVertexStream2svATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2svATI(GLenum stream, const GLshort *coords)"<Bar>echoh None<cr>
inorea glVertexStream2iATI glVertexStream2iATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2iATI(GLenum stream, GLint x, GLint y)"<Bar>echoh None<cr>
inorea glVertexStream2ivATI glVertexStream2ivATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2ivATI(GLenum stream, const GLint *coords)"<Bar>echoh None<cr>
inorea glVertexStream2fATI glVertexStream2fATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2fATI(GLenum stream, GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glVertexStream2fvATI glVertexStream2fvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2fvATI(GLenum stream, const GLfloat *coords)"<Bar>echoh None<cr>
inorea glVertexStream2dATI glVertexStream2dATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2dATI(GLenum stream, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertexStream2dvATI glVertexStream2dvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream2dvATI(GLenum stream, const GLdouble *coords)"<Bar>echoh None<cr>
inorea glVertexStream3sATI glVertexStream3sATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3sATI(GLenum stream, GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glVertexStream3svATI glVertexStream3svATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3svATI(GLenum stream, const GLshort *coords)"<Bar>echoh None<cr>
inorea glVertexStream3iATI glVertexStream3iATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3iATI(GLenum stream, GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glVertexStream3ivATI glVertexStream3ivATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3ivATI(GLenum stream, const GLint *coords)"<Bar>echoh None<cr>
inorea glVertexStream3fATI glVertexStream3fATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3fATI(GLenum stream, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glVertexStream3fvATI glVertexStream3fvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3fvATI(GLenum stream, const GLfloat *coords)"<Bar>echoh None<cr>
inorea glVertexStream3dATI glVertexStream3dATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3dATI(GLenum stream, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertexStream3dvATI glVertexStream3dvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream3dvATI(GLenum stream, const GLdouble *coords)"<Bar>echoh None<cr>
inorea glVertexStream4sATI glVertexStream4sATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4sATI(GLenum stream, GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glVertexStream4svATI glVertexStream4svATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4svATI(GLenum stream, const GLshort *coords)"<Bar>echoh None<cr>
inorea glVertexStream4iATI glVertexStream4iATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4iATI(GLenum stream, GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glVertexStream4ivATI glVertexStream4ivATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4ivATI(GLenum stream, const GLint *coords)"<Bar>echoh None<cr>
inorea glVertexStream4fATI glVertexStream4fATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4fATI(GLenum stream, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glVertexStream4fvATI glVertexStream4fvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4fvATI(GLenum stream, const GLfloat *coords)"<Bar>echoh None<cr>
inorea glVertexStream4dATI glVertexStream4dATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4dATI(GLenum stream, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertexStream4dvATI glVertexStream4dvATI<c-o>:echoh HintHL<Bar>echo "void glVertexStream4dvATI(GLenum stream, const GLdouble *coords)"<Bar>echoh None<cr>
inorea glNormalStream3bATI glNormalStream3bATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3bATI(GLenum stream, GLbyte nx, GLbyte ny, GLbyte nz)"<Bar>echoh None<cr>
inorea glNormalStream3bvATI glNormalStream3bvATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3bvATI(GLenum stream, const GLbyte *coords)"<Bar>echoh None<cr>
inorea glNormalStream3sATI glNormalStream3sATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3sATI(GLenum stream, GLshort nx, GLshort ny, GLshort nz)"<Bar>echoh None<cr>
inorea glNormalStream3svATI glNormalStream3svATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3svATI(GLenum stream, const GLshort *coords)"<Bar>echoh None<cr>
inorea glNormalStream3iATI glNormalStream3iATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3iATI(GLenum stream, GLint nx, GLint ny, GLint nz)"<Bar>echoh None<cr>
inorea glNormalStream3ivATI glNormalStream3ivATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3ivATI(GLenum stream, const GLint *coords)"<Bar>echoh None<cr>
inorea glNormalStream3fATI glNormalStream3fATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3fATI(GLenum stream, GLfloat nx, GLfloat ny, GLfloat nz)"<Bar>echoh None<cr>
inorea glNormalStream3fvATI glNormalStream3fvATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3fvATI(GLenum stream, const GLfloat *coords)"<Bar>echoh None<cr>
inorea glNormalStream3dATI glNormalStream3dATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3dATI(GLenum stream, GLdouble nx, GLdouble ny, GLdouble nz)"<Bar>echoh None<cr>
inorea glNormalStream3dvATI glNormalStream3dvATI<c-o>:echoh HintHL<Bar>echo "void glNormalStream3dvATI(GLenum stream, const GLdouble *coords)"<Bar>echoh None<cr>
inorea glClientActiveVertexStreamATI glClientActiveVertexStreamATI<c-o>:echoh HintHL<Bar>echo "void glClientActiveVertexStreamATI(GLenum stream)"<Bar>echoh None<cr>
inorea glVertexBlendEnviATI glVertexBlendEnviATI<c-o>:echoh HintHL<Bar>echo "void glVertexBlendEnviATI(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glVertexBlendEnvfATI glVertexBlendEnvfATI<c-o>:echoh HintHL<Bar>echo "void glVertexBlendEnvfATI(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glElementPointerATI glElementPointerATI<c-o>:echoh HintHL<Bar>echo "void glElementPointerATI(GLenum type, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glDrawElementArrayATI glDrawElementArrayATI<c-o>:echoh HintHL<Bar>echo "void glDrawElementArrayATI(GLenum mode, GLsizei count)"<Bar>echoh None<cr>
inorea glDrawRangeElementArrayATI glDrawRangeElementArrayATI<c-o>:echoh HintHL<Bar>echo "void glDrawRangeElementArrayATI(GLenum mode, GLuint start, GLuint end, GLsizei count)"<Bar>echoh None<cr>
inorea glDrawMeshArraysSUN glDrawMeshArraysSUN<c-o>:echoh HintHL<Bar>echo "void glDrawMeshArraysSUN(GLenum mode, GLint first, GLsizei count, GLsizei width)"<Bar>echoh None<cr>
inorea glGenOcclusionQueriesNV glGenOcclusionQueriesNV<c-o>:echoh HintHL<Bar>echo "void glGenOcclusionQueriesNV(GLsizei n, GLuint *ids)"<Bar>echoh None<cr>
inorea glDeleteOcclusionQueriesNV glDeleteOcclusionQueriesNV<c-o>:echoh HintHL<Bar>echo "void glDeleteOcclusionQueriesNV(GLsizei n, const GLuint *ids)"<Bar>echoh None<cr>
inorea glIsOcclusionQueryNV glIsOcclusionQueryNV<c-o>:echoh HintHL<Bar>echo "GLboolean glIsOcclusionQueryNV(GLuint id)"<Bar>echoh None<cr>
inorea glBeginOcclusionQueryNV glBeginOcclusionQueryNV<c-o>:echoh HintHL<Bar>echo "void glBeginOcclusionQueryNV(GLuint id)"<Bar>echoh None<cr>
inorea glEndOcclusionQueryNV glEndOcclusionQueryNV<c-o>:echoh HintHL<Bar>echo "void glEndOcclusionQueryNV(void)"<Bar>echoh None<cr>
inorea glGetOcclusionQueryivNV glGetOcclusionQueryivNV<c-o>:echoh HintHL<Bar>echo "void glGetOcclusionQueryivNV(GLuint id, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetOcclusionQueryuivNV glGetOcclusionQueryuivNV<c-o>:echoh HintHL<Bar>echo "void glGetOcclusionQueryuivNV(GLuint id, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glPointParameteriNV glPointParameteriNV<c-o>:echoh HintHL<Bar>echo "void glPointParameteriNV(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPointParameterivNV glPointParameterivNV<c-o>:echoh HintHL<Bar>echo "void glPointParameterivNV(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glActiveStencilFaceEXT glActiveStencilFaceEXT<c-o>:echoh HintHL<Bar>echo "void glActiveStencilFaceEXT(GLenum face)"<Bar>echoh None<cr>
inorea glElementPointerAPPLE glElementPointerAPPLE<c-o>:echoh HintHL<Bar>echo "void glElementPointerAPPLE(GLenum type, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glDrawElementArrayAPPLE glDrawElementArrayAPPLE<c-o>:echoh HintHL<Bar>echo "void glDrawElementArrayAPPLE(GLenum mode, GLint first, GLsizei count)"<Bar>echoh None<cr>
inorea glDrawRangeElementArrayAPPLE glDrawRangeElementArrayAPPLE<c-o>:echoh HintHL<Bar>echo "void glDrawRangeElementArrayAPPLE(GLenum mode, GLuint start, GLuint end, GLint first, GLsizei count)"<Bar>echoh None<cr>
inorea glMultiDrawElementArrayAPPLE glMultiDrawElementArrayAPPLE<c-o>:echoh HintHL<Bar>echo "void glMultiDrawElementArrayAPPLE(GLenum mode, const GLint *first, const GLsizei *count, GLsizei primcount)"<Bar>echoh None<cr>
inorea glMultiDrawRangeElementArrayAPPLE glMultiDrawRangeElementArrayAPPLE<c-o>:echoh HintHL<Bar>echo "void glMultiDrawRangeElementArrayAPPLE(GLenum mode, GLuint start, GLuint end, const GLint *first, const GLsizei *count, GLsizei primcount)"<Bar>echoh None<cr>
inorea glGenFencesAPPLE glGenFencesAPPLE<c-o>:echoh HintHL<Bar>echo "void glGenFencesAPPLE(GLsizei n, GLuint *fences)"<Bar>echoh None<cr>
inorea glDeleteFencesAPPLE glDeleteFencesAPPLE<c-o>:echoh HintHL<Bar>echo "void glDeleteFencesAPPLE(GLsizei n, const GLuint *fences)"<Bar>echoh None<cr>
inorea glSetFenceAPPLE glSetFenceAPPLE<c-o>:echoh HintHL<Bar>echo "void glSetFenceAPPLE(GLuint fence)"<Bar>echoh None<cr>
inorea glIsFenceAPPLE glIsFenceAPPLE<c-o>:echoh HintHL<Bar>echo "GLboolean glIsFenceAPPLE(GLuint fence)"<Bar>echoh None<cr>
inorea glTestFenceAPPLE glTestFenceAPPLE<c-o>:echoh HintHL<Bar>echo "GLboolean glTestFenceAPPLE(GLuint fence)"<Bar>echoh None<cr>
inorea glFinishFenceAPPLE glFinishFenceAPPLE<c-o>:echoh HintHL<Bar>echo "void glFinishFenceAPPLE(GLuint fence)"<Bar>echoh None<cr>
inorea glTestObjectAPPLE glTestObjectAPPLE<c-o>:echoh HintHL<Bar>echo "GLboolean glTestObjectAPPLE(GLenum object, GLuint name)"<Bar>echoh None<cr>
inorea glFinishObjectAPPLE glFinishObjectAPPLE<c-o>:echoh HintHL<Bar>echo "void glFinishObjectAPPLE(GLenum object, GLint name)"<Bar>echoh None<cr>
inorea glBindVertexArrayAPPLE glBindVertexArrayAPPLE<c-o>:echoh HintHL<Bar>echo "void glBindVertexArrayAPPLE(GLuint array)"<Bar>echoh None<cr>
inorea glDeleteVertexArraysAPPLE glDeleteVertexArraysAPPLE<c-o>:echoh HintHL<Bar>echo "void glDeleteVertexArraysAPPLE(GLsizei n, const GLuint *arrays)"<Bar>echoh None<cr>
inorea glGenVertexArraysAPPLE glGenVertexArraysAPPLE<c-o>:echoh HintHL<Bar>echo "void glGenVertexArraysAPPLE(GLsizei n, GLuint *arrays)"<Bar>echoh None<cr>
inorea glIsVertexArrayAPPLE glIsVertexArrayAPPLE<c-o>:echoh HintHL<Bar>echo "GLboolean glIsVertexArrayAPPLE(GLuint array)"<Bar>echoh None<cr>
inorea glVertexArrayRangeAPPLE glVertexArrayRangeAPPLE<c-o>:echoh HintHL<Bar>echo "void glVertexArrayRangeAPPLE(GLsizei length, GLvoid *pointer)"<Bar>echoh None<cr>
inorea glFlushVertexArrayRangeAPPLE glFlushVertexArrayRangeAPPLE<c-o>:echoh HintHL<Bar>echo "void glFlushVertexArrayRangeAPPLE(GLsizei length, GLvoid *pointer)"<Bar>echoh None<cr>
inorea glVertexArrayParameteriAPPLE glVertexArrayParameteriAPPLE<c-o>:echoh HintHL<Bar>echo "void glVertexArrayParameteriAPPLE(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glDrawBuffersATI glDrawBuffersATI<c-o>:echoh HintHL<Bar>echo "void glDrawBuffersATI(GLsizei n, const GLenum *bufs)"<Bar>echoh None<cr>
inorea glProgramNamedParameter4fNV glProgramNamedParameter4fNV<c-o>:echoh HintHL<Bar>echo "void glProgramNamedParameter4fNV(GLuint id, GLsizei len, const GLubyte *name, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glProgramNamedParameter4dNV glProgramNamedParameter4dNV<c-o>:echoh HintHL<Bar>echo "void glProgramNamedParameter4dNV(GLuint id, GLsizei len, const GLubyte *name, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glProgramNamedParameter4fvNV glProgramNamedParameter4fvNV<c-o>:echoh HintHL<Bar>echo "void glProgramNamedParameter4fvNV(GLuint id, GLsizei len, const GLubyte *name, const GLfloat *v)"<Bar>echoh None<cr>
inorea glProgramNamedParameter4dvNV glProgramNamedParameter4dvNV<c-o>:echoh HintHL<Bar>echo "void glProgramNamedParameter4dvNV(GLuint id, GLsizei len, const GLubyte *name, const GLdouble *v)"<Bar>echoh None<cr>
inorea glGetProgramNamedParameterfvNV glGetProgramNamedParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramNamedParameterfvNV(GLuint id, GLsizei len, const GLubyte *name, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetProgramNamedParameterdvNV glGetProgramNamedParameterdvNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramNamedParameterdvNV(GLuint id, GLsizei len, const GLubyte *name, GLdouble *params)"<Bar>echoh None<cr>
inorea glVertex2hNV glVertex2hNV<c-o>:echoh HintHL<Bar>echo "void glVertex2hNV(GLhalfNV x, GLhalfNV y)"<Bar>echoh None<cr>
inorea glVertex2hvNV glVertex2hvNV<c-o>:echoh HintHL<Bar>echo "void glVertex2hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertex3hNV glVertex3hNV<c-o>:echoh HintHL<Bar>echo "void glVertex3hNV(GLhalfNV x, GLhalfNV y, GLhalfNV z)"<Bar>echoh None<cr>
inorea glVertex3hvNV glVertex3hvNV<c-o>:echoh HintHL<Bar>echo "void glVertex3hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertex4hNV glVertex4hNV<c-o>:echoh HintHL<Bar>echo "void glVertex4hNV(GLhalfNV x, GLhalfNV y, GLhalfNV z, GLhalfNV w)"<Bar>echoh None<cr>
inorea glVertex4hvNV glVertex4hvNV<c-o>:echoh HintHL<Bar>echo "void glVertex4hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glNormal3hNV glNormal3hNV<c-o>:echoh HintHL<Bar>echo "void glNormal3hNV(GLhalfNV nx, GLhalfNV ny, GLhalfNV nz)"<Bar>echoh None<cr>
inorea glNormal3hvNV glNormal3hvNV<c-o>:echoh HintHL<Bar>echo "void glNormal3hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glColor3hNV glColor3hNV<c-o>:echoh HintHL<Bar>echo "void glColor3hNV(GLhalfNV red, GLhalfNV green, GLhalfNV blue)"<Bar>echoh None<cr>
inorea glColor3hvNV glColor3hvNV<c-o>:echoh HintHL<Bar>echo "void glColor3hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glColor4hNV glColor4hNV<c-o>:echoh HintHL<Bar>echo "void glColor4hNV(GLhalfNV red, GLhalfNV green, GLhalfNV blue, GLhalfNV alpha)"<Bar>echoh None<cr>
inorea glColor4hvNV glColor4hvNV<c-o>:echoh HintHL<Bar>echo "void glColor4hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glTexCoord1hNV glTexCoord1hNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord1hNV(GLhalfNV s)"<Bar>echoh None<cr>
inorea glTexCoord1hvNV glTexCoord1hvNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord1hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glTexCoord2hNV glTexCoord2hNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord2hNV(GLhalfNV s, GLhalfNV t)"<Bar>echoh None<cr>
inorea glTexCoord2hvNV glTexCoord2hvNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord2hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glTexCoord3hNV glTexCoord3hNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord3hNV(GLhalfNV s, GLhalfNV t, GLhalfNV r)"<Bar>echoh None<cr>
inorea glTexCoord3hvNV glTexCoord3hvNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord3hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glTexCoord4hNV glTexCoord4hNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord4hNV(GLhalfNV s, GLhalfNV t, GLhalfNV r, GLhalfNV q)"<Bar>echoh None<cr>
inorea glTexCoord4hvNV glTexCoord4hvNV<c-o>:echoh HintHL<Bar>echo "void glTexCoord4hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1hNV glMultiTexCoord1hNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1hNV(GLenum target, GLhalfNV s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1hvNV glMultiTexCoord1hvNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1hvNV(GLenum target, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2hNV glMultiTexCoord2hNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2hNV(GLenum target, GLhalfNV s, GLhalfNV t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2hvNV glMultiTexCoord2hvNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2hvNV(GLenum target, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3hNV glMultiTexCoord3hNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3hNV(GLenum target, GLhalfNV s, GLhalfNV t, GLhalfNV r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3hvNV glMultiTexCoord3hvNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3hvNV(GLenum target, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4hNV glMultiTexCoord4hNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4hNV(GLenum target, GLhalfNV s, GLhalfNV t, GLhalfNV r, GLhalfNV q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4hvNV glMultiTexCoord4hvNV<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4hvNV(GLenum target, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glFogCoordhNV glFogCoordhNV<c-o>:echoh HintHL<Bar>echo "void glFogCoordhNV(GLhalfNV fog)"<Bar>echoh None<cr>
inorea glFogCoordhvNV glFogCoordhvNV<c-o>:echoh HintHL<Bar>echo "void glFogCoordhvNV(const GLhalfNV *fog)"<Bar>echoh None<cr>
inorea glSecondaryColor3hNV glSecondaryColor3hNV<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3hNV(GLhalfNV red, GLhalfNV green, GLhalfNV blue)"<Bar>echoh None<cr>
inorea glSecondaryColor3hvNV glSecondaryColor3hvNV<c-o>:echoh HintHL<Bar>echo "void glSecondaryColor3hvNV(const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexWeighthNV glVertexWeighthNV<c-o>:echoh HintHL<Bar>echo "void glVertexWeighthNV(GLhalfNV weight)"<Bar>echoh None<cr>
inorea glVertexWeighthvNV glVertexWeighthvNV<c-o>:echoh HintHL<Bar>echo "void glVertexWeighthvNV(const GLhalfNV *weight)"<Bar>echoh None<cr>
inorea glVertexAttrib1hNV glVertexAttrib1hNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1hNV(GLuint index, GLhalfNV x)"<Bar>echoh None<cr>
inorea glVertexAttrib1hvNV glVertexAttrib1hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib1hvNV(GLuint index, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexAttrib2hNV glVertexAttrib2hNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2hNV(GLuint index, GLhalfNV x, GLhalfNV y)"<Bar>echoh None<cr>
inorea glVertexAttrib2hvNV glVertexAttrib2hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib2hvNV(GLuint index, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexAttrib3hNV glVertexAttrib3hNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3hNV(GLuint index, GLhalfNV x, GLhalfNV y, GLhalfNV z)"<Bar>echoh None<cr>
inorea glVertexAttrib3hvNV glVertexAttrib3hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib3hvNV(GLuint index, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexAttrib4hNV glVertexAttrib4hNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4hNV(GLuint index, GLhalfNV x, GLhalfNV y, GLhalfNV z, GLhalfNV w)"<Bar>echoh None<cr>
inorea glVertexAttrib4hvNV glVertexAttrib4hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttrib4hvNV(GLuint index, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexAttribs1hvNV glVertexAttribs1hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs1hvNV(GLuint index, GLsizei n, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexAttribs2hvNV glVertexAttribs2hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs2hvNV(GLuint index, GLsizei n, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexAttribs3hvNV glVertexAttribs3hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs3hvNV(GLuint index, GLsizei n, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glVertexAttribs4hvNV glVertexAttribs4hvNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribs4hvNV(GLuint index, GLsizei n, const GLhalfNV *v)"<Bar>echoh None<cr>
inorea glPixelDataRangeNV glPixelDataRangeNV<c-o>:echoh HintHL<Bar>echo "void glPixelDataRangeNV(GLenum target, GLsizei length, GLvoid *pointer)"<Bar>echoh None<cr>
inorea glFlushPixelDataRangeNV glFlushPixelDataRangeNV<c-o>:echoh HintHL<Bar>echo "void glFlushPixelDataRangeNV(GLenum target)"<Bar>echoh None<cr>
inorea glPrimitiveRestartNV glPrimitiveRestartNV<c-o>:echoh HintHL<Bar>echo "void glPrimitiveRestartNV(void)"<Bar>echoh None<cr>
inorea glPrimitiveRestartIndexNV glPrimitiveRestartIndexNV<c-o>:echoh HintHL<Bar>echo "void glPrimitiveRestartIndexNV(GLuint index)"<Bar>echoh None<cr>
inorea glMapObjectBufferATI glMapObjectBufferATI<c-o>:echoh HintHL<Bar>echo "GLvoid *glMapObjectBufferATI(GLuint buffer)"<Bar>echoh None<cr>
inorea glUnmapObjectBufferATI glUnmapObjectBufferATI<c-o>:echoh HintHL<Bar>echo "void glUnmapObjectBufferATI(GLuint buffer)"<Bar>echoh None<cr>
inorea glStencilOpSeparateATI glStencilOpSeparateATI<c-o>:echoh HintHL<Bar>echo "void glStencilOpSeparateATI(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass)"<Bar>echoh None<cr>
inorea glStencilFuncSeparateATI glStencilFuncSeparateATI<c-o>:echoh HintHL<Bar>echo "void glStencilFuncSeparateATI(GLenum frontfunc, GLenum backfunc, GLint ref, GLuint mask)"<Bar>echoh None<cr>
inorea glVertexAttribArrayObjectATI glVertexAttribArrayObjectATI<c-o>:echoh HintHL<Bar>echo "void glVertexAttribArrayObjectATI(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, GLuint buffer, GLuint offset)"<Bar>echoh None<cr>
inorea glGetVertexAttribArrayObjectfvATI glGetVertexAttribArrayObjectfvATI<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribArrayObjectfvATI(GLuint index, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribArrayObjectivATI glGetVertexAttribArrayObjectivATI<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribArrayObjectivATI(GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glDepthBoundsEXT glDepthBoundsEXT<c-o>:echoh HintHL<Bar>echo "void glDepthBoundsEXT(GLclampd zmin, GLclampd zmax)"<Bar>echoh None<cr>
inorea glBlendEquationSeparateEXT glBlendEquationSeparateEXT<c-o>:echoh HintHL<Bar>echo "void glBlendEquationSeparateEXT(GLenum modeRGB, GLenum modeAlpha)"<Bar>echoh None<cr>
inorea glIsRenderbufferEXT glIsRenderbufferEXT<c-o>:echoh HintHL<Bar>echo "GLboolean glIsRenderbufferEXT(GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glBindRenderbufferEXT glBindRenderbufferEXT<c-o>:echoh HintHL<Bar>echo "void glBindRenderbufferEXT(GLenum target, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glDeleteRenderbuffersEXT glDeleteRenderbuffersEXT<c-o>:echoh HintHL<Bar>echo "void glDeleteRenderbuffersEXT(GLsizei n, const GLuint *renderbuffers)"<Bar>echoh None<cr>
inorea glGenRenderbuffersEXT glGenRenderbuffersEXT<c-o>:echoh HintHL<Bar>echo "void glGenRenderbuffersEXT(GLsizei n, GLuint *renderbuffers)"<Bar>echoh None<cr>
inorea glRenderbufferStorageEXT glRenderbufferStorageEXT<c-o>:echoh HintHL<Bar>echo "void glRenderbufferStorageEXT(GLenum target, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetRenderbufferParameterivEXT glGetRenderbufferParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetRenderbufferParameterivEXT(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glIsFramebufferEXT glIsFramebufferEXT<c-o>:echoh HintHL<Bar>echo "GLboolean glIsFramebufferEXT(GLuint framebuffer)"<Bar>echoh None<cr>
inorea glBindFramebufferEXT glBindFramebufferEXT<c-o>:echoh HintHL<Bar>echo "void glBindFramebufferEXT(GLenum target, GLuint framebuffer)"<Bar>echoh None<cr>
inorea glDeleteFramebuffersEXT glDeleteFramebuffersEXT<c-o>:echoh HintHL<Bar>echo "void glDeleteFramebuffersEXT(GLsizei n, const GLuint *framebuffers)"<Bar>echoh None<cr>
inorea glGenFramebuffersEXT glGenFramebuffersEXT<c-o>:echoh HintHL<Bar>echo "void glGenFramebuffersEXT(GLsizei n, GLuint *framebuffers)"<Bar>echoh None<cr>
inorea glCheckFramebufferStatusEXT glCheckFramebufferStatusEXT<c-o>:echoh HintHL<Bar>echo "GLenum glCheckFramebufferStatusEXT(GLenum target)"<Bar>echoh None<cr>
inorea glFramebufferTexture1DEXT glFramebufferTexture1DEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferTexture1DEXT(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glFramebufferTexture2DEXT glFramebufferTexture2DEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferTexture2DEXT(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glFramebufferTexture3DEXT glFramebufferTexture3DEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferTexture3DEXT(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level, GLint zoffset)"<Bar>echoh None<cr>
inorea glFramebufferRenderbufferEXT glFramebufferRenderbufferEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferRenderbufferEXT(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glGetFramebufferAttachmentParameterivEXT glGetFramebufferAttachmentParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetFramebufferAttachmentParameterivEXT(GLenum target, GLenum attachment, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGenerateMipmapEXT glGenerateMipmapEXT<c-o>:echoh HintHL<Bar>echo "void glGenerateMipmapEXT(GLenum target)"<Bar>echoh None<cr>
inorea glStringMarkerGREMEDY glStringMarkerGREMEDY<c-o>:echoh HintHL<Bar>echo "void glStringMarkerGREMEDY(GLsizei len, const GLvoid *string)"<Bar>echoh None<cr>
inorea glStencilClearTagEXT glStencilClearTagEXT<c-o>:echoh HintHL<Bar>echo "void glStencilClearTagEXT(GLsizei stencilTagBits, GLuint stencilClearTag)"<Bar>echoh None<cr>
inorea glBlitFramebufferEXT glBlitFramebufferEXT<c-o>:echoh HintHL<Bar>echo "void glBlitFramebufferEXT(GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0, GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter)"<Bar>echoh None<cr>
inorea glRenderbufferStorageMultisampleEXT glRenderbufferStorageMultisampleEXT<c-o>:echoh HintHL<Bar>echo "void glRenderbufferStorageMultisampleEXT(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetQueryObjecti64vEXT glGetQueryObjecti64vEXT<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjecti64vEXT(GLuint id, GLenum pname, GLint64EXT *params)"<Bar>echoh None<cr>
inorea glGetQueryObjectui64vEXT glGetQueryObjectui64vEXT<c-o>:echoh HintHL<Bar>echo "void glGetQueryObjectui64vEXT(GLuint id, GLenum pname, GLuint64EXT *params)"<Bar>echoh None<cr>
inorea glProgramEnvParameters4fvEXT glProgramEnvParameters4fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameters4fvEXT(GLenum target, GLuint index, GLsizei count, const GLfloat *params)"<Bar>echoh None<cr>
inorea glProgramLocalParameters4fvEXT glProgramLocalParameters4fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameters4fvEXT(GLenum target, GLuint index, GLsizei count, const GLfloat *params)"<Bar>echoh None<cr>
inorea glBufferParameteriAPPLE glBufferParameteriAPPLE<c-o>:echoh HintHL<Bar>echo "void glBufferParameteriAPPLE(GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glFlushMappedBufferRangeAPPLE glFlushMappedBufferRangeAPPLE<c-o>:echoh HintHL<Bar>echo "void glFlushMappedBufferRangeAPPLE(GLenum target, GLintptr offset, GLsizeiptr size)"<Bar>echoh None<cr>
inorea glProgramLocalParameterI4iNV glProgramLocalParameterI4iNV<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameterI4iNV(GLenum target, GLuint index, GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glProgramLocalParameterI4ivNV glProgramLocalParameterI4ivNV<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameterI4ivNV(GLenum target, GLuint index, const GLint *params)"<Bar>echoh None<cr>
inorea glProgramLocalParametersI4ivNV glProgramLocalParametersI4ivNV<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParametersI4ivNV(GLenum target, GLuint index, GLsizei count, const GLint *params)"<Bar>echoh None<cr>
inorea glProgramLocalParameterI4uiNV glProgramLocalParameterI4uiNV<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameterI4uiNV(GLenum target, GLuint index, GLuint x, GLuint y, GLuint z, GLuint w)"<Bar>echoh None<cr>
inorea glProgramLocalParameterI4uivNV glProgramLocalParameterI4uivNV<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParameterI4uivNV(GLenum target, GLuint index, const GLuint *params)"<Bar>echoh None<cr>
inorea glProgramLocalParametersI4uivNV glProgramLocalParametersI4uivNV<c-o>:echoh HintHL<Bar>echo "void glProgramLocalParametersI4uivNV(GLenum target, GLuint index, GLsizei count, const GLuint *params)"<Bar>echoh None<cr>
inorea glProgramEnvParameterI4iNV glProgramEnvParameterI4iNV<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameterI4iNV(GLenum target, GLuint index, GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glProgramEnvParameterI4ivNV glProgramEnvParameterI4ivNV<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameterI4ivNV(GLenum target, GLuint index, const GLint *params)"<Bar>echoh None<cr>
inorea glProgramEnvParametersI4ivNV glProgramEnvParametersI4ivNV<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParametersI4ivNV(GLenum target, GLuint index, GLsizei count, const GLint *params)"<Bar>echoh None<cr>
inorea glProgramEnvParameterI4uiNV glProgramEnvParameterI4uiNV<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameterI4uiNV(GLenum target, GLuint index, GLuint x, GLuint y, GLuint z, GLuint w)"<Bar>echoh None<cr>
inorea glProgramEnvParameterI4uivNV glProgramEnvParameterI4uivNV<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParameterI4uivNV(GLenum target, GLuint index, const GLuint *params)"<Bar>echoh None<cr>
inorea glProgramEnvParametersI4uivNV glProgramEnvParametersI4uivNV<c-o>:echoh HintHL<Bar>echo "void glProgramEnvParametersI4uivNV(GLenum target, GLuint index, GLsizei count, const GLuint *params)"<Bar>echoh None<cr>
inorea glGetProgramLocalParameterIivNV glGetProgramLocalParameterIivNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramLocalParameterIivNV(GLenum target, GLuint index, GLint *params)"<Bar>echoh None<cr>
inorea glGetProgramLocalParameterIuivNV glGetProgramLocalParameterIuivNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramLocalParameterIuivNV(GLenum target, GLuint index, GLuint *params)"<Bar>echoh None<cr>
inorea glGetProgramEnvParameterIivNV glGetProgramEnvParameterIivNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramEnvParameterIivNV(GLenum target, GLuint index, GLint *params)"<Bar>echoh None<cr>
inorea glGetProgramEnvParameterIuivNV glGetProgramEnvParameterIuivNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramEnvParameterIuivNV(GLenum target, GLuint index, GLuint *params)"<Bar>echoh None<cr>
inorea glProgramVertexLimitNV glProgramVertexLimitNV<c-o>:echoh HintHL<Bar>echo "void glProgramVertexLimitNV(GLenum target, GLint limit)"<Bar>echoh None<cr>
inorea glFramebufferTextureEXT glFramebufferTextureEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferTextureEXT(GLenum target, GLenum attachment, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glFramebufferTextureLayerEXT glFramebufferTextureLayerEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferTextureLayerEXT(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer)"<Bar>echoh None<cr>
inorea glFramebufferTextureFaceEXT glFramebufferTextureFaceEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferTextureFaceEXT(GLenum target, GLenum attachment, GLuint texture, GLint level, GLenum face)"<Bar>echoh None<cr>
inorea glProgramParameteriEXT glProgramParameteriEXT<c-o>:echoh HintHL<Bar>echo "void glProgramParameteriEXT(GLuint program, GLenum pname, GLint value)"<Bar>echoh None<cr>
inorea glVertexAttribI1iEXT glVertexAttribI1iEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1iEXT(GLuint index, GLint x)"<Bar>echoh None<cr>
inorea glVertexAttribI2iEXT glVertexAttribI2iEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2iEXT(GLuint index, GLint x, GLint y)"<Bar>echoh None<cr>
inorea glVertexAttribI3iEXT glVertexAttribI3iEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3iEXT(GLuint index, GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glVertexAttribI4iEXT glVertexAttribI4iEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4iEXT(GLuint index, GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glVertexAttribI1uiEXT glVertexAttribI1uiEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1uiEXT(GLuint index, GLuint x)"<Bar>echoh None<cr>
inorea glVertexAttribI2uiEXT glVertexAttribI2uiEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2uiEXT(GLuint index, GLuint x, GLuint y)"<Bar>echoh None<cr>
inorea glVertexAttribI3uiEXT glVertexAttribI3uiEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3uiEXT(GLuint index, GLuint x, GLuint y, GLuint z)"<Bar>echoh None<cr>
inorea glVertexAttribI4uiEXT glVertexAttribI4uiEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4uiEXT(GLuint index, GLuint x, GLuint y, GLuint z, GLuint w)"<Bar>echoh None<cr>
inorea glVertexAttribI1ivEXT glVertexAttribI1ivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1ivEXT(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI2ivEXT glVertexAttribI2ivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2ivEXT(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI3ivEXT glVertexAttribI3ivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3ivEXT(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4ivEXT glVertexAttribI4ivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4ivEXT(GLuint index, const GLint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI1uivEXT glVertexAttribI1uivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI1uivEXT(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI2uivEXT glVertexAttribI2uivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI2uivEXT(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI3uivEXT glVertexAttribI3uivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI3uivEXT(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4uivEXT glVertexAttribI4uivEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4uivEXT(GLuint index, const GLuint *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4bvEXT glVertexAttribI4bvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4bvEXT(GLuint index, const GLbyte *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4svEXT glVertexAttribI4svEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4svEXT(GLuint index, const GLshort *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4ubvEXT glVertexAttribI4ubvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4ubvEXT(GLuint index, const GLubyte *v)"<Bar>echoh None<cr>
inorea glVertexAttribI4usvEXT glVertexAttribI4usvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribI4usvEXT(GLuint index, const GLushort *v)"<Bar>echoh None<cr>
inorea glVertexAttribIPointerEXT glVertexAttribIPointerEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribIPointerEXT(GLuint index, GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glGetVertexAttribIivEXT glGetVertexAttribIivEXT<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribIivEXT(GLuint index, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribIuivEXT glGetVertexAttribIuivEXT<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribIuivEXT(GLuint index, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glGetUniformuivEXT glGetUniformuivEXT<c-o>:echoh HintHL<Bar>echo "void glGetUniformuivEXT(GLuint program, GLint location, GLuint *params)"<Bar>echoh None<cr>
inorea glBindFragDataLocationEXT glBindFragDataLocationEXT<c-o>:echoh HintHL<Bar>echo "void glBindFragDataLocationEXT(GLuint program, GLuint color, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetFragDataLocationEXT glGetFragDataLocationEXT<c-o>:echoh HintHL<Bar>echo "GLint glGetFragDataLocationEXT(GLuint program, const GLchar *name)"<Bar>echoh None<cr>
inorea glUniform1uiEXT glUniform1uiEXT<c-o>:echoh HintHL<Bar>echo "void glUniform1uiEXT(GLint location, GLuint v0)"<Bar>echoh None<cr>
inorea glUniform2uiEXT glUniform2uiEXT<c-o>:echoh HintHL<Bar>echo "void glUniform2uiEXT(GLint location, GLuint v0, GLuint v1)"<Bar>echoh None<cr>
inorea glUniform3uiEXT glUniform3uiEXT<c-o>:echoh HintHL<Bar>echo "void glUniform3uiEXT(GLint location, GLuint v0, GLuint v1, GLuint v2)"<Bar>echoh None<cr>
inorea glUniform4uiEXT glUniform4uiEXT<c-o>:echoh HintHL<Bar>echo "void glUniform4uiEXT(GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3)"<Bar>echoh None<cr>
inorea glUniform1uivEXT glUniform1uivEXT<c-o>:echoh HintHL<Bar>echo "void glUniform1uivEXT(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glUniform2uivEXT glUniform2uivEXT<c-o>:echoh HintHL<Bar>echo "void glUniform2uivEXT(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glUniform3uivEXT glUniform3uivEXT<c-o>:echoh HintHL<Bar>echo "void glUniform3uivEXT(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glUniform4uivEXT glUniform4uivEXT<c-o>:echoh HintHL<Bar>echo "void glUniform4uivEXT(GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glDrawArraysInstancedEXT glDrawArraysInstancedEXT<c-o>:echoh HintHL<Bar>echo "void glDrawArraysInstancedEXT(GLenum mode, GLint start, GLsizei count, GLsizei primcount)"<Bar>echoh None<cr>
inorea glDrawElementsInstancedEXT glDrawElementsInstancedEXT<c-o>:echoh HintHL<Bar>echo "void glDrawElementsInstancedEXT(GLenum mode, GLsizei count, GLenum type, const GLvoid *indices, GLsizei primcount)"<Bar>echoh None<cr>
inorea glTexBufferEXT glTexBufferEXT<c-o>:echoh HintHL<Bar>echo "void glTexBufferEXT(GLenum target, GLenum internalformat, GLuint buffer)"<Bar>echoh None<cr>
inorea glDepthRangedNV glDepthRangedNV<c-o>:echoh HintHL<Bar>echo "void glDepthRangedNV(GLdouble zNear, GLdouble zFar)"<Bar>echoh None<cr>
inorea glClearDepthdNV glClearDepthdNV<c-o>:echoh HintHL<Bar>echo "void glClearDepthdNV(GLdouble depth)"<Bar>echoh None<cr>
inorea glDepthBoundsdNV glDepthBoundsdNV<c-o>:echoh HintHL<Bar>echo "void glDepthBoundsdNV(GLdouble zmin, GLdouble zmax)"<Bar>echoh None<cr>
inorea glRenderbufferStorageMultisampleCoverageNV glRenderbufferStorageMultisampleCoverageNV<c-o>:echoh HintHL<Bar>echo "void glRenderbufferStorageMultisampleCoverageNV(GLenum target, GLsizei coverageSamples, GLsizei colorSamples, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glProgramBufferParametersfvNV glProgramBufferParametersfvNV<c-o>:echoh HintHL<Bar>echo "void glProgramBufferParametersfvNV(GLenum target, GLuint buffer, GLuint index, GLsizei count, const GLfloat *params)"<Bar>echoh None<cr>
inorea glProgramBufferParametersIivNV glProgramBufferParametersIivNV<c-o>:echoh HintHL<Bar>echo "void glProgramBufferParametersIivNV(GLenum target, GLuint buffer, GLuint index, GLsizei count, const GLint *params)"<Bar>echoh None<cr>
inorea glProgramBufferParametersIuivNV glProgramBufferParametersIuivNV<c-o>:echoh HintHL<Bar>echo "void glProgramBufferParametersIuivNV(GLenum target, GLuint buffer, GLuint index, GLsizei count, const GLuint *params)"<Bar>echoh None<cr>
inorea glColorMaskIndexedEXT glColorMaskIndexedEXT<c-o>:echoh HintHL<Bar>echo "void glColorMaskIndexedEXT(GLuint index, GLboolean r, GLboolean g, GLboolean b, GLboolean a)"<Bar>echoh None<cr>
inorea glGetBooleanIndexedvEXT glGetBooleanIndexedvEXT<c-o>:echoh HintHL<Bar>echo "void glGetBooleanIndexedvEXT(GLenum target, GLuint index, GLboolean *data)"<Bar>echoh None<cr>
inorea glGetIntegerIndexedvEXT glGetIntegerIndexedvEXT<c-o>:echoh HintHL<Bar>echo "void glGetIntegerIndexedvEXT(GLenum target, GLuint index, GLint *data)"<Bar>echoh None<cr>
inorea glEnableIndexedEXT glEnableIndexedEXT<c-o>:echoh HintHL<Bar>echo "void glEnableIndexedEXT(GLenum target, GLuint index)"<Bar>echoh None<cr>
inorea glDisableIndexedEXT glDisableIndexedEXT<c-o>:echoh HintHL<Bar>echo "void glDisableIndexedEXT(GLenum target, GLuint index)"<Bar>echoh None<cr>
inorea glIsEnabledIndexedEXT glIsEnabledIndexedEXT<c-o>:echoh HintHL<Bar>echo "GLboolean glIsEnabledIndexedEXT(GLenum target, GLuint index)"<Bar>echoh None<cr>
inorea glBeginTransformFeedbackNV glBeginTransformFeedbackNV<c-o>:echoh HintHL<Bar>echo "void glBeginTransformFeedbackNV(GLenum primitiveMode)"<Bar>echoh None<cr>
inorea glEndTransformFeedbackNV glEndTransformFeedbackNV<c-o>:echoh HintHL<Bar>echo "void glEndTransformFeedbackNV(void)"<Bar>echoh None<cr>
inorea glTransformFeedbackAttribsNV glTransformFeedbackAttribsNV<c-o>:echoh HintHL<Bar>echo "void glTransformFeedbackAttribsNV(GLuint count, const GLint *attribs, GLenum bufferMode)"<Bar>echoh None<cr>
inorea glBindBufferRangeNV glBindBufferRangeNV<c-o>:echoh HintHL<Bar>echo "void glBindBufferRangeNV(GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size)"<Bar>echoh None<cr>
inorea glBindBufferOffsetNV glBindBufferOffsetNV<c-o>:echoh HintHL<Bar>echo "void glBindBufferOffsetNV(GLenum target, GLuint index, GLuint buffer, GLintptr offset)"<Bar>echoh None<cr>
inorea glBindBufferBaseNV glBindBufferBaseNV<c-o>:echoh HintHL<Bar>echo "void glBindBufferBaseNV(GLenum target, GLuint index, GLuint buffer)"<Bar>echoh None<cr>
inorea glTransformFeedbackVaryingsNV glTransformFeedbackVaryingsNV<c-o>:echoh HintHL<Bar>echo "void glTransformFeedbackVaryingsNV(GLuint program, GLsizei count, const GLint *locations, GLenum bufferMode)"<Bar>echoh None<cr>
inorea glActiveVaryingNV glActiveVaryingNV<c-o>:echoh HintHL<Bar>echo "void glActiveVaryingNV(GLuint program, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetVaryingLocationNV glGetVaryingLocationNV<c-o>:echoh HintHL<Bar>echo "GLint glGetVaryingLocationNV(GLuint program, const GLchar *name)"<Bar>echoh None<cr>
inorea glGetActiveVaryingNV glGetActiveVaryingNV<c-o>:echoh HintHL<Bar>echo "void glGetActiveVaryingNV(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLsizei *size, GLenum *type, GLchar *name)"<Bar>echoh None<cr>
inorea glGetTransformFeedbackVaryingNV glGetTransformFeedbackVaryingNV<c-o>:echoh HintHL<Bar>echo "void glGetTransformFeedbackVaryingNV(GLuint program, GLuint index, GLint *location)"<Bar>echoh None<cr>
inorea glTransformFeedbackStreamAttribsNV glTransformFeedbackStreamAttribsNV<c-o>:echoh HintHL<Bar>echo "void glTransformFeedbackStreamAttribsNV(GLsizei count, const GLint *attribs, GLsizei nbuffers, const GLint *bufstreams, GLenum bufferMode)"<Bar>echoh None<cr>
inorea glUniformBufferEXT glUniformBufferEXT<c-o>:echoh HintHL<Bar>echo "void glUniformBufferEXT(GLuint program, GLint location, GLuint buffer)"<Bar>echoh None<cr>
inorea glGetUniformBufferSizeEXT glGetUniformBufferSizeEXT<c-o>:echoh HintHL<Bar>echo "GLint glGetUniformBufferSizeEXT(GLuint program, GLint location)"<Bar>echoh None<cr>
inorea glGetUniformOffsetEXT glGetUniformOffsetEXT<c-o>:echoh HintHL<Bar>echo "GLintptr glGetUniformOffsetEXT(GLuint program, GLint location)"<Bar>echoh None<cr>
inorea glTexParameterIivEXT glTexParameterIivEXT<c-o>:echoh HintHL<Bar>echo "void glTexParameterIivEXT(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glTexParameterIuivEXT glTexParameterIuivEXT<c-o>:echoh HintHL<Bar>echo "void glTexParameterIuivEXT(GLenum target, GLenum pname, const GLuint *params)"<Bar>echoh None<cr>
inorea glGetTexParameterIivEXT glGetTexParameterIivEXT<c-o>:echoh HintHL<Bar>echo "void glGetTexParameterIivEXT(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTexParameterIuivEXT glGetTexParameterIuivEXT<c-o>:echoh HintHL<Bar>echo "void glGetTexParameterIuivEXT(GLenum target, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glClearColorIiEXT glClearColorIiEXT<c-o>:echoh HintHL<Bar>echo "void glClearColorIiEXT(GLint red, GLint green, GLint blue, GLint alpha)"<Bar>echoh None<cr>
inorea glClearColorIuiEXT glClearColorIuiEXT<c-o>:echoh HintHL<Bar>echo "void glClearColorIuiEXT(GLuint red, GLuint green, GLuint blue, GLuint alpha)"<Bar>echoh None<cr>
inorea glFrameTerminatorGREMEDY glFrameTerminatorGREMEDY<c-o>:echoh HintHL<Bar>echo "void glFrameTerminatorGREMEDY(void)"<Bar>echoh None<cr>
inorea glBeginConditionalRenderNV glBeginConditionalRenderNV<c-o>:echoh HintHL<Bar>echo "void glBeginConditionalRenderNV(GLuint id, GLenum mode)"<Bar>echoh None<cr>
inorea glEndConditionalRenderNV glEndConditionalRenderNV<c-o>:echoh HintHL<Bar>echo "void glEndConditionalRenderNV(void)"<Bar>echoh None<cr>
inorea glPresentFrameKeyedNV glPresentFrameKeyedNV<c-o>:echoh HintHL<Bar>echo "void glPresentFrameKeyedNV(GLuint video_slot, GLuint64EXT minPresentTime, GLuint beginPresentTimeId, GLuint presentDurationId, GLenum type, GLenum target0, GLuint fill0, GLuint key0, GLenum target1, GLuint fill1, GLuint key1)"<Bar>echoh None<cr>
inorea glPresentFrameDualFillNV glPresentFrameDualFillNV<c-o>:echoh HintHL<Bar>echo "void glPresentFrameDualFillNV(GLuint video_slot, GLuint64EXT minPresentTime, GLuint beginPresentTimeId, GLuint presentDurationId, GLenum type, GLenum target0, GLuint fill0, GLenum target1, GLuint fill1, GLenum target2, GLuint fill2, GLenum target3, GLuint fill3)"<Bar>echoh None<cr>
inorea glGetVideoivNV glGetVideoivNV<c-o>:echoh HintHL<Bar>echo "void glGetVideoivNV(GLuint video_slot, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVideouivNV glGetVideouivNV<c-o>:echoh HintHL<Bar>echo "void glGetVideouivNV(GLuint video_slot, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glGetVideoi64vNV glGetVideoi64vNV<c-o>:echoh HintHL<Bar>echo "void glGetVideoi64vNV(GLuint video_slot, GLenum pname, GLint64EXT *params)"<Bar>echoh None<cr>
inorea glGetVideoui64vNV glGetVideoui64vNV<c-o>:echoh HintHL<Bar>echo "void glGetVideoui64vNV(GLuint video_slot, GLenum pname, GLuint64EXT *params)"<Bar>echoh None<cr>
inorea glBeginTransformFeedbackEXT glBeginTransformFeedbackEXT<c-o>:echoh HintHL<Bar>echo "void glBeginTransformFeedbackEXT(GLenum primitiveMode)"<Bar>echoh None<cr>
inorea glEndTransformFeedbackEXT glEndTransformFeedbackEXT<c-o>:echoh HintHL<Bar>echo "void glEndTransformFeedbackEXT(void)"<Bar>echoh None<cr>
inorea glBindBufferRangeEXT glBindBufferRangeEXT<c-o>:echoh HintHL<Bar>echo "void glBindBufferRangeEXT(GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size)"<Bar>echoh None<cr>
inorea glBindBufferOffsetEXT glBindBufferOffsetEXT<c-o>:echoh HintHL<Bar>echo "void glBindBufferOffsetEXT(GLenum target, GLuint index, GLuint buffer, GLintptr offset)"<Bar>echoh None<cr>
inorea glBindBufferBaseEXT glBindBufferBaseEXT<c-o>:echoh HintHL<Bar>echo "void glBindBufferBaseEXT(GLenum target, GLuint index, GLuint buffer)"<Bar>echoh None<cr>
inorea glTransformFeedbackVaryingsEXT glTransformFeedbackVaryingsEXT<c-o>:echoh HintHL<Bar>echo "void glTransformFeedbackVaryingsEXT(GLuint program, GLsizei count, const GLchar* *varyings, GLenum bufferMode)"<Bar>echoh None<cr>
inorea glGetTransformFeedbackVaryingEXT glGetTransformFeedbackVaryingEXT<c-o>:echoh HintHL<Bar>echo "void glGetTransformFeedbackVaryingEXT(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLsizei *size, GLenum *type, GLchar *name)"<Bar>echoh None<cr>
inorea glClientAttribDefaultEXT glClientAttribDefaultEXT<c-o>:echoh HintHL<Bar>echo "void glClientAttribDefaultEXT(GLbitfield mask)"<Bar>echoh None<cr>
inorea glPushClientAttribDefaultEXT glPushClientAttribDefaultEXT<c-o>:echoh HintHL<Bar>echo "void glPushClientAttribDefaultEXT(GLbitfield mask)"<Bar>echoh None<cr>
inorea glMatrixLoadfEXT glMatrixLoadfEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixLoadfEXT(GLenum mode, const GLfloat *m)"<Bar>echoh None<cr>
inorea glMatrixLoaddEXT glMatrixLoaddEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixLoaddEXT(GLenum mode, const GLdouble *m)"<Bar>echoh None<cr>
inorea glMatrixMultfEXT glMatrixMultfEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixMultfEXT(GLenum mode, const GLfloat *m)"<Bar>echoh None<cr>
inorea glMatrixMultdEXT glMatrixMultdEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixMultdEXT(GLenum mode, const GLdouble *m)"<Bar>echoh None<cr>
inorea glMatrixLoadIdentityEXT glMatrixLoadIdentityEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixLoadIdentityEXT(GLenum mode)"<Bar>echoh None<cr>
inorea glMatrixRotatefEXT glMatrixRotatefEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixRotatefEXT(GLenum mode, GLfloat angle, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glMatrixRotatedEXT glMatrixRotatedEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixRotatedEXT(GLenum mode, GLdouble angle, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glMatrixScalefEXT glMatrixScalefEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixScalefEXT(GLenum mode, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glMatrixScaledEXT glMatrixScaledEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixScaledEXT(GLenum mode, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glMatrixTranslatefEXT glMatrixTranslatefEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixTranslatefEXT(GLenum mode, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glMatrixTranslatedEXT glMatrixTranslatedEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixTranslatedEXT(GLenum mode, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glMatrixFrustumEXT glMatrixFrustumEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixFrustumEXT(GLenum mode, GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)"<Bar>echoh None<cr>
inorea glMatrixOrthoEXT glMatrixOrthoEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixOrthoEXT(GLenum mode, GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)"<Bar>echoh None<cr>
inorea glMatrixPopEXT glMatrixPopEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixPopEXT(GLenum mode)"<Bar>echoh None<cr>
inorea glMatrixPushEXT glMatrixPushEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixPushEXT(GLenum mode)"<Bar>echoh None<cr>
inorea glMatrixLoadTransposefEXT glMatrixLoadTransposefEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixLoadTransposefEXT(GLenum mode, const GLfloat *m)"<Bar>echoh None<cr>
inorea glMatrixLoadTransposedEXT glMatrixLoadTransposedEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixLoadTransposedEXT(GLenum mode, const GLdouble *m)"<Bar>echoh None<cr>
inorea glMatrixMultTransposefEXT glMatrixMultTransposefEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixMultTransposefEXT(GLenum mode, const GLfloat *m)"<Bar>echoh None<cr>
inorea glMatrixMultTransposedEXT glMatrixMultTransposedEXT<c-o>:echoh HintHL<Bar>echo "void glMatrixMultTransposedEXT(GLenum mode, const GLdouble *m)"<Bar>echoh None<cr>
inorea glTextureParameterfEXT glTextureParameterfEXT<c-o>:echoh HintHL<Bar>echo "void glTextureParameterfEXT(GLuint texture, GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glTextureParameterfvEXT glTextureParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glTextureParameterfvEXT(GLuint texture, GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glTextureParameteriEXT glTextureParameteriEXT<c-o>:echoh HintHL<Bar>echo "void glTextureParameteriEXT(GLuint texture, GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glTextureParameterivEXT glTextureParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glTextureParameterivEXT(GLuint texture, GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glTextureImage1DEXT glTextureImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glTextureImage1DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTextureImage2DEXT glTextureImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glTextureImage2DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTextureSubImage1DEXT glTextureSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glTextureSubImage1DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTextureSubImage2DEXT glTextureSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glTextureSubImage2DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glCopyTextureImage1DEXT glCopyTextureImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTextureImage1DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)"<Bar>echoh None<cr>
inorea glCopyTextureImage2DEXT glCopyTextureImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTextureImage2DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)"<Bar>echoh None<cr>
inorea glCopyTextureSubImage1DEXT glCopyTextureSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTextureSubImage1DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyTextureSubImage2DEXT glCopyTextureSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTextureSubImage2DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetTextureImageEXT glGetTextureImageEXT<c-o>:echoh HintHL<Bar>echo "void glGetTextureImageEXT(GLuint texture, GLenum target, GLint level, GLenum format, GLenum type, GLvoid *pixels)"<Bar>echoh None<cr>
inorea glGetTextureParameterfvEXT glGetTextureParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetTextureParameterfvEXT(GLuint texture, GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTextureParameterivEXT glGetTextureParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetTextureParameterivEXT(GLuint texture, GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTextureLevelParameterfvEXT glGetTextureLevelParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetTextureLevelParameterfvEXT(GLuint texture, GLenum target, GLint level, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTextureLevelParameterivEXT glGetTextureLevelParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetTextureLevelParameterivEXT(GLuint texture, GLenum target, GLint level, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glTextureImage3DEXT glTextureImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glTextureImage3DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glTextureSubImage3DEXT glTextureSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glTextureSubImage3DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glCopyTextureSubImage3DEXT glCopyTextureSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyTextureSubImage3DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glMultiTexParameterfEXT glMultiTexParameterfEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexParameterfEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glMultiTexParameterfvEXT glMultiTexParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexParameterfvEXT(GLenum texunit, GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glMultiTexParameteriEXT glMultiTexParameteriEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexParameteriEXT(GLenum texunit, GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glMultiTexParameterivEXT glMultiTexParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexParameterivEXT(GLenum texunit, GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glMultiTexImage1DEXT glMultiTexImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexImage1DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glMultiTexImage2DEXT glMultiTexImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexImage2DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glMultiTexSubImage1DEXT glMultiTexSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexSubImage1DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glMultiTexSubImage2DEXT glMultiTexSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexSubImage2DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glCopyMultiTexImage1DEXT glCopyMultiTexImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyMultiTexImage1DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)"<Bar>echoh None<cr>
inorea glCopyMultiTexImage2DEXT glCopyMultiTexImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyMultiTexImage2DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)"<Bar>echoh None<cr>
inorea glCopyMultiTexSubImage1DEXT glCopyMultiTexSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyMultiTexSubImage1DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyMultiTexSubImage2DEXT glCopyMultiTexSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyMultiTexSubImage2DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetMultiTexImageEXT glGetMultiTexImageEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexImageEXT(GLenum texunit, GLenum target, GLint level, GLenum format, GLenum type, GLvoid *pixels)"<Bar>echoh None<cr>
inorea glGetMultiTexParameterfvEXT glGetMultiTexParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexParameterfvEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMultiTexParameterivEXT glGetMultiTexParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexParameterivEXT(GLenum texunit, GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMultiTexLevelParameterfvEXT glGetMultiTexLevelParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexLevelParameterfvEXT(GLenum texunit, GLenum target, GLint level, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMultiTexLevelParameterivEXT glGetMultiTexLevelParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexLevelParameterivEXT(GLenum texunit, GLenum target, GLint level, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glMultiTexImage3DEXT glMultiTexImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexImage3DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glMultiTexSubImage3DEXT glMultiTexSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexSubImage3DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *pixels)"<Bar>echoh None<cr>
inorea glCopyMultiTexSubImage3DEXT glCopyMultiTexSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glCopyMultiTexSubImage3DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glBindMultiTextureEXT glBindMultiTextureEXT<c-o>:echoh HintHL<Bar>echo "void glBindMultiTextureEXT(GLenum texunit, GLenum target, GLuint texture)"<Bar>echoh None<cr>
inorea glEnableClientStateIndexedEXT glEnableClientStateIndexedEXT<c-o>:echoh HintHL<Bar>echo "void glEnableClientStateIndexedEXT(GLenum array, GLuint index)"<Bar>echoh None<cr>
inorea glDisableClientStateIndexedEXT glDisableClientStateIndexedEXT<c-o>:echoh HintHL<Bar>echo "void glDisableClientStateIndexedEXT(GLenum array, GLuint index)"<Bar>echoh None<cr>
inorea glMultiTexCoordPointerEXT glMultiTexCoordPointerEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoordPointerEXT(GLenum texunit, GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glMultiTexEnvfEXT glMultiTexEnvfEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexEnvfEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glMultiTexEnvfvEXT glMultiTexEnvfvEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexEnvfvEXT(GLenum texunit, GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glMultiTexEnviEXT glMultiTexEnviEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexEnviEXT(GLenum texunit, GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glMultiTexEnvivEXT glMultiTexEnvivEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexEnvivEXT(GLenum texunit, GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glMultiTexGendEXT glMultiTexGendEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexGendEXT(GLenum texunit, GLenum coord, GLenum pname, GLdouble param)"<Bar>echoh None<cr>
inorea glMultiTexGendvEXT glMultiTexGendvEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexGendvEXT(GLenum texunit, GLenum coord, GLenum pname, const GLdouble *params)"<Bar>echoh None<cr>
inorea glMultiTexGenfEXT glMultiTexGenfEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexGenfEXT(GLenum texunit, GLenum coord, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glMultiTexGenfvEXT glMultiTexGenfvEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexGenfvEXT(GLenum texunit, GLenum coord, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glMultiTexGeniEXT glMultiTexGeniEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexGeniEXT(GLenum texunit, GLenum coord, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glMultiTexGenivEXT glMultiTexGenivEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexGenivEXT(GLenum texunit, GLenum coord, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glGetMultiTexEnvfvEXT glGetMultiTexEnvfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexEnvfvEXT(GLenum texunit, GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMultiTexEnvivEXT glGetMultiTexEnvivEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexEnvivEXT(GLenum texunit, GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMultiTexGendvEXT glGetMultiTexGendvEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexGendvEXT(GLenum texunit, GLenum coord, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetMultiTexGenfvEXT glGetMultiTexGenfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexGenfvEXT(GLenum texunit, GLenum coord, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMultiTexGenivEXT glGetMultiTexGenivEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexGenivEXT(GLenum texunit, GLenum coord, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetFloatIndexedvEXT glGetFloatIndexedvEXT<c-o>:echoh HintHL<Bar>echo "void glGetFloatIndexedvEXT(GLenum target, GLuint index, GLfloat *data)"<Bar>echoh None<cr>
inorea glGetDoubleIndexedvEXT glGetDoubleIndexedvEXT<c-o>:echoh HintHL<Bar>echo "void glGetDoubleIndexedvEXT(GLenum target, GLuint index, GLdouble *data)"<Bar>echoh None<cr>
inorea glGetPointerIndexedvEXT glGetPointerIndexedvEXT<c-o>:echoh HintHL<Bar>echo "void glGetPointerIndexedvEXT(GLenum target, GLuint index, GLvoid* *data)"<Bar>echoh None<cr>
inorea glCompressedTextureImage3DEXT glCompressedTextureImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedTextureImage3DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedTextureImage2DEXT glCompressedTextureImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedTextureImage2DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedTextureImage1DEXT glCompressedTextureImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedTextureImage1DEXT(GLuint texture, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedTextureSubImage3DEXT glCompressedTextureSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedTextureSubImage3DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedTextureSubImage2DEXT glCompressedTextureSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedTextureSubImage2DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedTextureSubImage1DEXT glCompressedTextureSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedTextureSubImage1DEXT(GLuint texture, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glGetCompressedTextureImageEXT glGetCompressedTextureImageEXT<c-o>:echoh HintHL<Bar>echo "void glGetCompressedTextureImageEXT(GLuint texture, GLenum target, GLint lod, GLvoid *img)"<Bar>echoh None<cr>
inorea glCompressedMultiTexImage3DEXT glCompressedMultiTexImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedMultiTexImage3DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedMultiTexImage2DEXT glCompressedMultiTexImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedMultiTexImage2DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedMultiTexImage1DEXT glCompressedMultiTexImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedMultiTexImage1DEXT(GLenum texunit, GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedMultiTexSubImage3DEXT glCompressedMultiTexSubImage3DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedMultiTexSubImage3DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedMultiTexSubImage2DEXT glCompressedMultiTexSubImage2DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedMultiTexSubImage2DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glCompressedMultiTexSubImage1DEXT glCompressedMultiTexSubImage1DEXT<c-o>:echoh HintHL<Bar>echo "void glCompressedMultiTexSubImage1DEXT(GLenum texunit, GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *bits)"<Bar>echoh None<cr>
inorea glGetCompressedMultiTexImageEXT glGetCompressedMultiTexImageEXT<c-o>:echoh HintHL<Bar>echo "void glGetCompressedMultiTexImageEXT(GLenum texunit, GLenum target, GLint lod, GLvoid *img)"<Bar>echoh None<cr>
inorea glNamedProgramStringEXT glNamedProgramStringEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramStringEXT(GLuint program, GLenum target, GLenum format, GLsizei len, const GLvoid *string)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameter4dEXT glNamedProgramLocalParameter4dEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameter4dEXT(GLuint program, GLenum target, GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameter4dvEXT glNamedProgramLocalParameter4dvEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameter4dvEXT(GLuint program, GLenum target, GLuint index, const GLdouble *params)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameter4fEXT glNamedProgramLocalParameter4fEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameter4fEXT(GLuint program, GLenum target, GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameter4fvEXT glNamedProgramLocalParameter4fvEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameter4fvEXT(GLuint program, GLenum target, GLuint index, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetNamedProgramLocalParameterdvEXT glGetNamedProgramLocalParameterdvEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedProgramLocalParameterdvEXT(GLuint program, GLenum target, GLuint index, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetNamedProgramLocalParameterfvEXT glGetNamedProgramLocalParameterfvEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedProgramLocalParameterfvEXT(GLuint program, GLenum target, GLuint index, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetNamedProgramivEXT glGetNamedProgramivEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedProgramivEXT(GLuint program, GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetNamedProgramStringEXT glGetNamedProgramStringEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedProgramStringEXT(GLuint program, GLenum target, GLenum pname, GLvoid *string)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameters4fvEXT glNamedProgramLocalParameters4fvEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameters4fvEXT(GLuint program, GLenum target, GLuint index, GLsizei count, const GLfloat *params)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameterI4iEXT glNamedProgramLocalParameterI4iEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameterI4iEXT(GLuint program, GLenum target, GLuint index, GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameterI4ivEXT glNamedProgramLocalParameterI4ivEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameterI4ivEXT(GLuint program, GLenum target, GLuint index, const GLint *params)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParametersI4ivEXT glNamedProgramLocalParametersI4ivEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParametersI4ivEXT(GLuint program, GLenum target, GLuint index, GLsizei count, const GLint *params)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameterI4uiEXT glNamedProgramLocalParameterI4uiEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameterI4uiEXT(GLuint program, GLenum target, GLuint index, GLuint x, GLuint y, GLuint z, GLuint w)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParameterI4uivEXT glNamedProgramLocalParameterI4uivEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParameterI4uivEXT(GLuint program, GLenum target, GLuint index, const GLuint *params)"<Bar>echoh None<cr>
inorea glNamedProgramLocalParametersI4uivEXT glNamedProgramLocalParametersI4uivEXT<c-o>:echoh HintHL<Bar>echo "void glNamedProgramLocalParametersI4uivEXT(GLuint program, GLenum target, GLuint index, GLsizei count, const GLuint *params)"<Bar>echoh None<cr>
inorea glGetNamedProgramLocalParameterIivEXT glGetNamedProgramLocalParameterIivEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedProgramLocalParameterIivEXT(GLuint program, GLenum target, GLuint index, GLint *params)"<Bar>echoh None<cr>
inorea glGetNamedProgramLocalParameterIuivEXT glGetNamedProgramLocalParameterIuivEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedProgramLocalParameterIuivEXT(GLuint program, GLenum target, GLuint index, GLuint *params)"<Bar>echoh None<cr>
inorea glTextureParameterIivEXT glTextureParameterIivEXT<c-o>:echoh HintHL<Bar>echo "void glTextureParameterIivEXT(GLuint texture, GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glTextureParameterIuivEXT glTextureParameterIuivEXT<c-o>:echoh HintHL<Bar>echo "void glTextureParameterIuivEXT(GLuint texture, GLenum target, GLenum pname, const GLuint *params)"<Bar>echoh None<cr>
inorea glGetTextureParameterIivEXT glGetTextureParameterIivEXT<c-o>:echoh HintHL<Bar>echo "void glGetTextureParameterIivEXT(GLuint texture, GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTextureParameterIuivEXT glGetTextureParameterIuivEXT<c-o>:echoh HintHL<Bar>echo "void glGetTextureParameterIuivEXT(GLuint texture, GLenum target, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glMultiTexParameterIivEXT glMultiTexParameterIivEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexParameterIivEXT(GLenum texunit, GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glMultiTexParameterIuivEXT glMultiTexParameterIuivEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexParameterIuivEXT(GLenum texunit, GLenum target, GLenum pname, const GLuint *params)"<Bar>echoh None<cr>
inorea glGetMultiTexParameterIivEXT glGetMultiTexParameterIivEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexParameterIivEXT(GLenum texunit, GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetMultiTexParameterIuivEXT glGetMultiTexParameterIuivEXT<c-o>:echoh HintHL<Bar>echo "void glGetMultiTexParameterIuivEXT(GLenum texunit, GLenum target, GLenum pname, GLuint *params)"<Bar>echoh None<cr>
inorea glProgramUniform1fEXT glProgramUniform1fEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1fEXT(GLuint program, GLint location, GLfloat v0)"<Bar>echoh None<cr>
inorea glProgramUniform2fEXT glProgramUniform2fEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2fEXT(GLuint program, GLint location, GLfloat v0, GLfloat v1)"<Bar>echoh None<cr>
inorea glProgramUniform3fEXT glProgramUniform3fEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3fEXT(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2)"<Bar>echoh None<cr>
inorea glProgramUniform4fEXT glProgramUniform4fEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4fEXT(GLuint program, GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)"<Bar>echoh None<cr>
inorea glProgramUniform1iEXT glProgramUniform1iEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1iEXT(GLuint program, GLint location, GLint v0)"<Bar>echoh None<cr>
inorea glProgramUniform2iEXT glProgramUniform2iEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2iEXT(GLuint program, GLint location, GLint v0, GLint v1)"<Bar>echoh None<cr>
inorea glProgramUniform3iEXT glProgramUniform3iEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3iEXT(GLuint program, GLint location, GLint v0, GLint v1, GLint v2)"<Bar>echoh None<cr>
inorea glProgramUniform4iEXT glProgramUniform4iEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4iEXT(GLuint program, GLint location, GLint v0, GLint v1, GLint v2, GLint v3)"<Bar>echoh None<cr>
inorea glProgramUniform1fvEXT glProgramUniform1fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform2fvEXT glProgramUniform2fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform3fvEXT glProgramUniform3fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform4fvEXT glProgramUniform4fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform1ivEXT glProgramUniform1ivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniform2ivEXT glProgramUniform2ivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniform3ivEXT glProgramUniform3ivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniform4ivEXT glProgramUniform4ivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2fvEXT glProgramUniformMatrix2fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3fvEXT glProgramUniformMatrix3fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4fvEXT glProgramUniformMatrix4fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x3fvEXT glProgramUniformMatrix2x3fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x3fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x2fvEXT glProgramUniformMatrix3x2fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x2fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x4fvEXT glProgramUniformMatrix2x4fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x4fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x2fvEXT glProgramUniformMatrix4x2fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x2fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x4fvEXT glProgramUniformMatrix3x4fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x4fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x3fvEXT glProgramUniformMatrix4x3fvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x3fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)"<Bar>echoh None<cr>
inorea glProgramUniform1uiEXT glProgramUniform1uiEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1uiEXT(GLuint program, GLint location, GLuint v0)"<Bar>echoh None<cr>
inorea glProgramUniform2uiEXT glProgramUniform2uiEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2uiEXT(GLuint program, GLint location, GLuint v0, GLuint v1)"<Bar>echoh None<cr>
inorea glProgramUniform3uiEXT glProgramUniform3uiEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3uiEXT(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2)"<Bar>echoh None<cr>
inorea glProgramUniform4uiEXT glProgramUniform4uiEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4uiEXT(GLuint program, GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3)"<Bar>echoh None<cr>
inorea glProgramUniform1uivEXT glProgramUniform1uivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glProgramUniform2uivEXT glProgramUniform2uivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glProgramUniform3uivEXT glProgramUniform3uivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glProgramUniform4uivEXT glProgramUniform4uivEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4uivEXT(GLuint program, GLint location, GLsizei count, const GLuint *value)"<Bar>echoh None<cr>
inorea glNamedBufferDataEXT glNamedBufferDataEXT<c-o>:echoh HintHL<Bar>echo "void glNamedBufferDataEXT(GLuint buffer, GLsizeiptr size, const GLvoid *data, GLenum usage)"<Bar>echoh None<cr>
inorea glNamedBufferSubDataEXT glNamedBufferSubDataEXT<c-o>:echoh HintHL<Bar>echo "void glNamedBufferSubDataEXT(GLuint buffer, GLintptr offset, GLsizeiptr size, const GLvoid *data)"<Bar>echoh None<cr>
inorea glMapNamedBufferEXT glMapNamedBufferEXT<c-o>:echoh HintHL<Bar>echo "GLvoid *glMapNamedBufferEXT(GLuint buffer, GLenum access)"<Bar>echoh None<cr>
inorea glUnmapNamedBufferEXT glUnmapNamedBufferEXT<c-o>:echoh HintHL<Bar>echo "GLboolean glUnmapNamedBufferEXT(GLuint buffer)"<Bar>echoh None<cr>
inorea glMapNamedBufferRangeEXT glMapNamedBufferRangeEXT<c-o>:echoh HintHL<Bar>echo "GLvoid *glMapNamedBufferRangeEXT(GLuint buffer, GLintptr offset, GLsizeiptr length, GLbitfield access)"<Bar>echoh None<cr>
inorea glFlushMappedNamedBufferRangeEXT glFlushMappedNamedBufferRangeEXT<c-o>:echoh HintHL<Bar>echo "void glFlushMappedNamedBufferRangeEXT(GLuint buffer, GLintptr offset, GLsizeiptr length)"<Bar>echoh None<cr>
inorea glNamedCopyBufferSubDataEXT glNamedCopyBufferSubDataEXT<c-o>:echoh HintHL<Bar>echo "void glNamedCopyBufferSubDataEXT(GLuint readBuffer, GLuint writeBuffer, GLintptr readOffset, GLintptr writeOffset, GLsizeiptr size)"<Bar>echoh None<cr>
inorea glGetNamedBufferParameterivEXT glGetNamedBufferParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedBufferParameterivEXT(GLuint buffer, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetNamedBufferPointervEXT glGetNamedBufferPointervEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedBufferPointervEXT(GLuint buffer, GLenum pname, GLvoid* *params)"<Bar>echoh None<cr>
inorea glGetNamedBufferSubDataEXT glGetNamedBufferSubDataEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedBufferSubDataEXT(GLuint buffer, GLintptr offset, GLsizeiptr size, GLvoid *data)"<Bar>echoh None<cr>
inorea glTextureBufferEXT glTextureBufferEXT<c-o>:echoh HintHL<Bar>echo "void glTextureBufferEXT(GLuint texture, GLenum target, GLenum internalformat, GLuint buffer)"<Bar>echoh None<cr>
inorea glMultiTexBufferEXT glMultiTexBufferEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexBufferEXT(GLenum texunit, GLenum target, GLenum internalformat, GLuint buffer)"<Bar>echoh None<cr>
inorea glNamedRenderbufferStorageEXT glNamedRenderbufferStorageEXT<c-o>:echoh HintHL<Bar>echo "void glNamedRenderbufferStorageEXT(GLuint renderbuffer, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetNamedRenderbufferParameterivEXT glGetNamedRenderbufferParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedRenderbufferParameterivEXT(GLuint renderbuffer, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glCheckNamedFramebufferStatusEXT glCheckNamedFramebufferStatusEXT<c-o>:echoh HintHL<Bar>echo "GLenum glCheckNamedFramebufferStatusEXT(GLuint framebuffer, GLenum target)"<Bar>echoh None<cr>
inorea glNamedFramebufferTexture1DEXT glNamedFramebufferTexture1DEXT<c-o>:echoh HintHL<Bar>echo "void glNamedFramebufferTexture1DEXT(GLuint framebuffer, GLenum attachment, GLenum textarget, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glNamedFramebufferTexture2DEXT glNamedFramebufferTexture2DEXT<c-o>:echoh HintHL<Bar>echo "void glNamedFramebufferTexture2DEXT(GLuint framebuffer, GLenum attachment, GLenum textarget, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glNamedFramebufferTexture3DEXT glNamedFramebufferTexture3DEXT<c-o>:echoh HintHL<Bar>echo "void glNamedFramebufferTexture3DEXT(GLuint framebuffer, GLenum attachment, GLenum textarget, GLuint texture, GLint level, GLint zoffset)"<Bar>echoh None<cr>
inorea glNamedFramebufferRenderbufferEXT glNamedFramebufferRenderbufferEXT<c-o>:echoh HintHL<Bar>echo "void glNamedFramebufferRenderbufferEXT(GLuint framebuffer, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glGetNamedFramebufferAttachmentParameterivEXT glGetNamedFramebufferAttachmentParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetNamedFramebufferAttachmentParameterivEXT(GLuint framebuffer, GLenum attachment, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGenerateTextureMipmapEXT glGenerateTextureMipmapEXT<c-o>:echoh HintHL<Bar>echo "void glGenerateTextureMipmapEXT(GLuint texture, GLenum target)"<Bar>echoh None<cr>
inorea glGenerateMultiTexMipmapEXT glGenerateMultiTexMipmapEXT<c-o>:echoh HintHL<Bar>echo "void glGenerateMultiTexMipmapEXT(GLenum texunit, GLenum target)"<Bar>echoh None<cr>
inorea glFramebufferDrawBufferEXT glFramebufferDrawBufferEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferDrawBufferEXT(GLuint framebuffer, GLenum mode)"<Bar>echoh None<cr>
inorea glFramebufferDrawBuffersEXT glFramebufferDrawBuffersEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferDrawBuffersEXT(GLuint framebuffer, GLsizei n, const GLenum *bufs)"<Bar>echoh None<cr>
inorea glFramebufferReadBufferEXT glFramebufferReadBufferEXT<c-o>:echoh HintHL<Bar>echo "void glFramebufferReadBufferEXT(GLuint framebuffer, GLenum mode)"<Bar>echoh None<cr>
inorea glGetFramebufferParameterivEXT glGetFramebufferParameterivEXT<c-o>:echoh HintHL<Bar>echo "void glGetFramebufferParameterivEXT(GLuint framebuffer, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glNamedRenderbufferStorageMultisampleEXT glNamedRenderbufferStorageMultisampleEXT<c-o>:echoh HintHL<Bar>echo "void glNamedRenderbufferStorageMultisampleEXT(GLuint renderbuffer, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glNamedRenderbufferStorageMultisampleCoverageEXT glNamedRenderbufferStorageMultisampleCoverageEXT<c-o>:echoh HintHL<Bar>echo "void glNamedRenderbufferStorageMultisampleCoverageEXT(GLuint renderbuffer, GLsizei coverageSamples, GLsizei colorSamples, GLenum internalformat, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glNamedFramebufferTextureEXT glNamedFramebufferTextureEXT<c-o>:echoh HintHL<Bar>echo "void glNamedFramebufferTextureEXT(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level)"<Bar>echoh None<cr>
inorea glNamedFramebufferTextureLayerEXT glNamedFramebufferTextureLayerEXT<c-o>:echoh HintHL<Bar>echo "void glNamedFramebufferTextureLayerEXT(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level, GLint layer)"<Bar>echoh None<cr>
inorea glNamedFramebufferTextureFaceEXT glNamedFramebufferTextureFaceEXT<c-o>:echoh HintHL<Bar>echo "void glNamedFramebufferTextureFaceEXT(GLuint framebuffer, GLenum attachment, GLuint texture, GLint level, GLenum face)"<Bar>echoh None<cr>
inorea glTextureRenderbufferEXT glTextureRenderbufferEXT<c-o>:echoh HintHL<Bar>echo "void glTextureRenderbufferEXT(GLuint texture, GLenum target, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glMultiTexRenderbufferEXT glMultiTexRenderbufferEXT<c-o>:echoh HintHL<Bar>echo "void glMultiTexRenderbufferEXT(GLenum texunit, GLenum target, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glProgramUniform1dEXT glProgramUniform1dEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1dEXT(GLuint program, GLint location, GLdouble x)"<Bar>echoh None<cr>
inorea glProgramUniform2dEXT glProgramUniform2dEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2dEXT(GLuint program, GLint location, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glProgramUniform3dEXT glProgramUniform3dEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3dEXT(GLuint program, GLint location, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glProgramUniform4dEXT glProgramUniform4dEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4dEXT(GLuint program, GLint location, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glProgramUniform1dvEXT glProgramUniform1dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniform2dvEXT glProgramUniform2dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniform3dvEXT glProgramUniform3dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniform4dvEXT glProgramUniform4dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4dvEXT(GLuint program, GLint location, GLsizei count, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2dvEXT glProgramUniformMatrix2dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3dvEXT glProgramUniformMatrix3dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4dvEXT glProgramUniformMatrix4dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x3dvEXT glProgramUniformMatrix2x3dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x3dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix2x4dvEXT glProgramUniformMatrix2x4dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix2x4dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x2dvEXT glProgramUniformMatrix3x2dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x2dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix3x4dvEXT glProgramUniformMatrix3x4dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix3x4dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x2dvEXT glProgramUniformMatrix4x2dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x2dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glProgramUniformMatrix4x3dvEXT glProgramUniformMatrix4x3dvEXT<c-o>:echoh HintHL<Bar>echo "void glProgramUniformMatrix4x3dvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLdouble *value)"<Bar>echoh None<cr>
inorea glGetMultisamplefvNV glGetMultisamplefvNV<c-o>:echoh HintHL<Bar>echo "void glGetMultisamplefvNV(GLenum pname, GLuint index, GLfloat *val)"<Bar>echoh None<cr>
inorea glSampleMaskIndexedNV glSampleMaskIndexedNV<c-o>:echoh HintHL<Bar>echo "void glSampleMaskIndexedNV(GLuint index, GLbitfield mask)"<Bar>echoh None<cr>
inorea glTexRenderbufferNV glTexRenderbufferNV<c-o>:echoh HintHL<Bar>echo "void glTexRenderbufferNV(GLenum target, GLuint renderbuffer)"<Bar>echoh None<cr>
inorea glBindTransformFeedbackNV glBindTransformFeedbackNV<c-o>:echoh HintHL<Bar>echo "void glBindTransformFeedbackNV(GLenum target, GLuint id)"<Bar>echoh None<cr>
inorea glDeleteTransformFeedbacksNV glDeleteTransformFeedbacksNV<c-o>:echoh HintHL<Bar>echo "void glDeleteTransformFeedbacksNV(GLsizei n, const GLuint *ids)"<Bar>echoh None<cr>
inorea glGenTransformFeedbacksNV glGenTransformFeedbacksNV<c-o>:echoh HintHL<Bar>echo "void glGenTransformFeedbacksNV(GLsizei n, GLuint *ids)"<Bar>echoh None<cr>
inorea glIsTransformFeedbackNV glIsTransformFeedbackNV<c-o>:echoh HintHL<Bar>echo "GLboolean glIsTransformFeedbackNV(GLuint id)"<Bar>echoh None<cr>
inorea glPauseTransformFeedbackNV glPauseTransformFeedbackNV<c-o>:echoh HintHL<Bar>echo "void glPauseTransformFeedbackNV(void)"<Bar>echoh None<cr>
inorea glResumeTransformFeedbackNV glResumeTransformFeedbackNV<c-o>:echoh HintHL<Bar>echo "void glResumeTransformFeedbackNV(void)"<Bar>echoh None<cr>
inorea glDrawTransformFeedbackNV glDrawTransformFeedbackNV<c-o>:echoh HintHL<Bar>echo "void glDrawTransformFeedbackNV(GLenum mode, GLuint id)"<Bar>echoh None<cr>
inorea glGetPerfMonitorGroupsAMD glGetPerfMonitorGroupsAMD<c-o>:echoh HintHL<Bar>echo "void glGetPerfMonitorGroupsAMD(GLint *numGroups, GLsizei groupsSize, GLuint *groups)"<Bar>echoh None<cr>
inorea glGetPerfMonitorCountersAMD glGetPerfMonitorCountersAMD<c-o>:echoh HintHL<Bar>echo "void glGetPerfMonitorCountersAMD(GLuint group, GLint *numCounters, GLint *maxActiveCounters, GLsizei counterSize, GLuint *counters)"<Bar>echoh None<cr>
inorea glGetPerfMonitorGroupStringAMD glGetPerfMonitorGroupStringAMD<c-o>:echoh HintHL<Bar>echo "void glGetPerfMonitorGroupStringAMD(GLuint group, GLsizei bufSize, GLsizei *length, GLchar *groupString)"<Bar>echoh None<cr>
inorea glGetPerfMonitorCounterStringAMD glGetPerfMonitorCounterStringAMD<c-o>:echoh HintHL<Bar>echo "void glGetPerfMonitorCounterStringAMD(GLuint group, GLuint counter, GLsizei bufSize, GLsizei *length, GLchar *counterString)"<Bar>echoh None<cr>
inorea glGetPerfMonitorCounterInfoAMD glGetPerfMonitorCounterInfoAMD<c-o>:echoh HintHL<Bar>echo "void glGetPerfMonitorCounterInfoAMD(GLuint group, GLuint counter, GLenum pname, GLvoid *data)"<Bar>echoh None<cr>
inorea glGenPerfMonitorsAMD glGenPerfMonitorsAMD<c-o>:echoh HintHL<Bar>echo "void glGenPerfMonitorsAMD(GLsizei n, GLuint *monitors)"<Bar>echoh None<cr>
inorea glDeletePerfMonitorsAMD glDeletePerfMonitorsAMD<c-o>:echoh HintHL<Bar>echo "void glDeletePerfMonitorsAMD(GLsizei n, GLuint *monitors)"<Bar>echoh None<cr>
inorea glSelectPerfMonitorCountersAMD glSelectPerfMonitorCountersAMD<c-o>:echoh HintHL<Bar>echo "void glSelectPerfMonitorCountersAMD(GLuint monitor, GLboolean enable, GLuint group, GLint numCounters, GLuint *counterList)"<Bar>echoh None<cr>
inorea glBeginPerfMonitorAMD glBeginPerfMonitorAMD<c-o>:echoh HintHL<Bar>echo "void glBeginPerfMonitorAMD(GLuint monitor)"<Bar>echoh None<cr>
inorea glEndPerfMonitorAMD glEndPerfMonitorAMD<c-o>:echoh HintHL<Bar>echo "void glEndPerfMonitorAMD(GLuint monitor)"<Bar>echoh None<cr>
inorea glGetPerfMonitorCounterDataAMD glGetPerfMonitorCounterDataAMD<c-o>:echoh HintHL<Bar>echo "void glGetPerfMonitorCounterDataAMD(GLuint monitor, GLenum pname, GLsizei dataSize, GLuint *data, GLint *bytesWritten)"<Bar>echoh None<cr>
inorea glTessellationFactorAMD glTessellationFactorAMD<c-o>:echoh HintHL<Bar>echo "void glTessellationFactorAMD(GLfloat factor)"<Bar>echoh None<cr>
inorea glTessellationModeAMD glTessellationModeAMD<c-o>:echoh HintHL<Bar>echo "void glTessellationModeAMD(GLenum mode)"<Bar>echoh None<cr>
inorea glProvokingVertexEXT glProvokingVertexEXT<c-o>:echoh HintHL<Bar>echo "void glProvokingVertexEXT(GLenum mode)"<Bar>echoh None<cr>
inorea glBlendFuncIndexedAMD glBlendFuncIndexedAMD<c-o>:echoh HintHL<Bar>echo "void glBlendFuncIndexedAMD(GLuint buf, GLenum src, GLenum dst)"<Bar>echoh None<cr>
inorea glBlendFuncSeparateIndexedAMD glBlendFuncSeparateIndexedAMD<c-o>:echoh HintHL<Bar>echo "void glBlendFuncSeparateIndexedAMD(GLuint buf, GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha)"<Bar>echoh None<cr>
inorea glBlendEquationIndexedAMD glBlendEquationIndexedAMD<c-o>:echoh HintHL<Bar>echo "void glBlendEquationIndexedAMD(GLuint buf, GLenum mode)"<Bar>echoh None<cr>
inorea glBlendEquationSeparateIndexedAMD glBlendEquationSeparateIndexedAMD<c-o>:echoh HintHL<Bar>echo "void glBlendEquationSeparateIndexedAMD(GLuint buf, GLenum modeRGB, GLenum modeAlpha)"<Bar>echoh None<cr>
inorea glTextureRangeAPPLE glTextureRangeAPPLE<c-o>:echoh HintHL<Bar>echo "void glTextureRangeAPPLE(GLenum target, GLsizei length, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glGetTexParameterPointervAPPLE glGetTexParameterPointervAPPLE<c-o>:echoh HintHL<Bar>echo "void glGetTexParameterPointervAPPLE(GLenum target, GLenum pname, GLvoid* *params)"<Bar>echoh None<cr>
inorea glEnableVertexAttribAPPLE glEnableVertexAttribAPPLE<c-o>:echoh HintHL<Bar>echo "void glEnableVertexAttribAPPLE(GLuint index, GLenum pname)"<Bar>echoh None<cr>
inorea glDisableVertexAttribAPPLE glDisableVertexAttribAPPLE<c-o>:echoh HintHL<Bar>echo "void glDisableVertexAttribAPPLE(GLuint index, GLenum pname)"<Bar>echoh None<cr>
inorea glIsVertexAttribEnabledAPPLE glIsVertexAttribEnabledAPPLE<c-o>:echoh HintHL<Bar>echo "GLboolean glIsVertexAttribEnabledAPPLE(GLuint index, GLenum pname)"<Bar>echoh None<cr>
inorea glMapVertexAttrib1dAPPLE glMapVertexAttrib1dAPPLE<c-o>:echoh HintHL<Bar>echo "void glMapVertexAttrib1dAPPLE(GLuint index, GLuint size, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points)"<Bar>echoh None<cr>
inorea glMapVertexAttrib1fAPPLE glMapVertexAttrib1fAPPLE<c-o>:echoh HintHL<Bar>echo "void glMapVertexAttrib1fAPPLE(GLuint index, GLuint size, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points)"<Bar>echoh None<cr>
inorea glMapVertexAttrib2dAPPLE glMapVertexAttrib2dAPPLE<c-o>:echoh HintHL<Bar>echo "void glMapVertexAttrib2dAPPLE(GLuint index, GLuint size, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points)"<Bar>echoh None<cr>
inorea glMapVertexAttrib2fAPPLE glMapVertexAttrib2fAPPLE<c-o>:echoh HintHL<Bar>echo "void glMapVertexAttrib2fAPPLE(GLuint index, GLuint size, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points)"<Bar>echoh None<cr>
inorea glObjectPurgeableAPPLE glObjectPurgeableAPPLE<c-o>:echoh HintHL<Bar>echo "GLenum glObjectPurgeableAPPLE(GLenum objectType, GLuint name, GLenum option)"<Bar>echoh None<cr>
inorea glObjectUnpurgeableAPPLE glObjectUnpurgeableAPPLE<c-o>:echoh HintHL<Bar>echo "GLenum glObjectUnpurgeableAPPLE(GLenum objectType, GLuint name, GLenum option)"<Bar>echoh None<cr>
inorea glGetObjectParameterivAPPLE glGetObjectParameterivAPPLE<c-o>:echoh HintHL<Bar>echo "void glGetObjectParameterivAPPLE(GLenum objectType, GLuint name, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glBeginVideoCaptureNV glBeginVideoCaptureNV<c-o>:echoh HintHL<Bar>echo "void glBeginVideoCaptureNV(GLuint video_capture_slot)"<Bar>echoh None<cr>
inorea glBindVideoCaptureStreamBufferNV glBindVideoCaptureStreamBufferNV<c-o>:echoh HintHL<Bar>echo "void glBindVideoCaptureStreamBufferNV(GLuint video_capture_slot, GLuint stream, GLenum frame_region, GLintptrARB offset)"<Bar>echoh None<cr>
inorea glBindVideoCaptureStreamTextureNV glBindVideoCaptureStreamTextureNV<c-o>:echoh HintHL<Bar>echo "void glBindVideoCaptureStreamTextureNV(GLuint video_capture_slot, GLuint stream, GLenum frame_region, GLenum target, GLuint texture)"<Bar>echoh None<cr>
inorea glEndVideoCaptureNV glEndVideoCaptureNV<c-o>:echoh HintHL<Bar>echo "void glEndVideoCaptureNV(GLuint video_capture_slot)"<Bar>echoh None<cr>
inorea glGetVideoCaptureivNV glGetVideoCaptureivNV<c-o>:echoh HintHL<Bar>echo "void glGetVideoCaptureivNV(GLuint video_capture_slot, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVideoCaptureStreamivNV glGetVideoCaptureStreamivNV<c-o>:echoh HintHL<Bar>echo "void glGetVideoCaptureStreamivNV(GLuint video_capture_slot, GLuint stream, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetVideoCaptureStreamfvNV glGetVideoCaptureStreamfvNV<c-o>:echoh HintHL<Bar>echo "void glGetVideoCaptureStreamfvNV(GLuint video_capture_slot, GLuint stream, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetVideoCaptureStreamdvNV glGetVideoCaptureStreamdvNV<c-o>:echoh HintHL<Bar>echo "void glGetVideoCaptureStreamdvNV(GLuint video_capture_slot, GLuint stream, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glVideoCaptureNV glVideoCaptureNV<c-o>:echoh HintHL<Bar>echo "GLenum glVideoCaptureNV(GLuint video_capture_slot, GLuint *sequence_num, GLuint64EXT *capture_time)"<Bar>echoh None<cr>
inorea glVideoCaptureStreamParameterivNV glVideoCaptureStreamParameterivNV<c-o>:echoh HintHL<Bar>echo "void glVideoCaptureStreamParameterivNV(GLuint video_capture_slot, GLuint stream, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glVideoCaptureStreamParameterfvNV glVideoCaptureStreamParameterfvNV<c-o>:echoh HintHL<Bar>echo "void glVideoCaptureStreamParameterfvNV(GLuint video_capture_slot, GLuint stream, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glVideoCaptureStreamParameterdvNV glVideoCaptureStreamParameterdvNV<c-o>:echoh HintHL<Bar>echo "void glVideoCaptureStreamParameterdvNV(GLuint video_capture_slot, GLuint stream, GLenum pname, const GLdouble *params)"<Bar>echoh None<cr>
inorea glCopyImageSubDataNV glCopyImageSubDataNV<c-o>:echoh HintHL<Bar>echo "void glCopyImageSubDataNV(GLuint srcName, GLenum srcTarget, GLint srcLevel, GLint srcX, GLint srcY, GLint srcZ, GLuint dstName, GLenum dstTarget, GLint dstLevel, GLint dstX, GLint dstY, GLint dstZ, GLsizei width, GLsizei height, GLsizei depth)"<Bar>echoh None<cr>
inorea glUseShaderProgramEXT glUseShaderProgramEXT<c-o>:echoh HintHL<Bar>echo "void glUseShaderProgramEXT(GLenum type, GLuint program)"<Bar>echoh None<cr>
inorea glActiveProgramEXT glActiveProgramEXT<c-o>:echoh HintHL<Bar>echo "void glActiveProgramEXT(GLuint program)"<Bar>echoh None<cr>
inorea glCreateShaderProgramEXT glCreateShaderProgramEXT<c-o>:echoh HintHL<Bar>echo "GLuint glCreateShaderProgramEXT(GLenum type, const GLchar *string)"<Bar>echoh None<cr>
inorea glMakeBufferResidentNV glMakeBufferResidentNV<c-o>:echoh HintHL<Bar>echo "void glMakeBufferResidentNV(GLenum target, GLenum access)"<Bar>echoh None<cr>
inorea glMakeBufferNonResidentNV glMakeBufferNonResidentNV<c-o>:echoh HintHL<Bar>echo "void glMakeBufferNonResidentNV(GLenum target)"<Bar>echoh None<cr>
inorea glIsBufferResidentNV glIsBufferResidentNV<c-o>:echoh HintHL<Bar>echo "GLboolean glIsBufferResidentNV(GLenum target)"<Bar>echoh None<cr>
inorea glMakeNamedBufferResidentNV glMakeNamedBufferResidentNV<c-o>:echoh HintHL<Bar>echo "void glMakeNamedBufferResidentNV(GLuint buffer, GLenum access)"<Bar>echoh None<cr>
inorea glMakeNamedBufferNonResidentNV glMakeNamedBufferNonResidentNV<c-o>:echoh HintHL<Bar>echo "void glMakeNamedBufferNonResidentNV(GLuint buffer)"<Bar>echoh None<cr>
inorea glIsNamedBufferResidentNV glIsNamedBufferResidentNV<c-o>:echoh HintHL<Bar>echo "GLboolean glIsNamedBufferResidentNV(GLuint buffer)"<Bar>echoh None<cr>
inorea glGetBufferParameterui64vNV glGetBufferParameterui64vNV<c-o>:echoh HintHL<Bar>echo "void glGetBufferParameterui64vNV(GLenum target, GLenum pname, GLuint64EXT *params)"<Bar>echoh None<cr>
inorea glGetNamedBufferParameterui64vNV glGetNamedBufferParameterui64vNV<c-o>:echoh HintHL<Bar>echo "void glGetNamedBufferParameterui64vNV(GLuint buffer, GLenum pname, GLuint64EXT *params)"<Bar>echoh None<cr>
inorea glGetIntegerui64vNV glGetIntegerui64vNV<c-o>:echoh HintHL<Bar>echo "void glGetIntegerui64vNV(GLenum value, GLuint64EXT *result)"<Bar>echoh None<cr>
inorea glUniformui64NV glUniformui64NV<c-o>:echoh HintHL<Bar>echo "void glUniformui64NV(GLint location, GLuint64EXT value)"<Bar>echoh None<cr>
inorea glUniformui64vNV glUniformui64vNV<c-o>:echoh HintHL<Bar>echo "void glUniformui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glGetUniformui64vNV glGetUniformui64vNV<c-o>:echoh HintHL<Bar>echo "void glGetUniformui64vNV(GLuint program, GLint location, GLuint64EXT *params)"<Bar>echoh None<cr>
inorea glProgramUniformui64NV glProgramUniformui64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniformui64NV(GLuint program, GLint location, GLuint64EXT value)"<Bar>echoh None<cr>
inorea glProgramUniformui64vNV glProgramUniformui64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniformui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glBufferAddressRangeNV glBufferAddressRangeNV<c-o>:echoh HintHL<Bar>echo "void glBufferAddressRangeNV(GLenum pname, GLuint index, GLuint64EXT address, GLsizeiptr length)"<Bar>echoh None<cr>
inorea glVertexFormatNV glVertexFormatNV<c-o>:echoh HintHL<Bar>echo "void glVertexFormatNV(GLint size, GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glNormalFormatNV glNormalFormatNV<c-o>:echoh HintHL<Bar>echo "void glNormalFormatNV(GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glColorFormatNV glColorFormatNV<c-o>:echoh HintHL<Bar>echo "void glColorFormatNV(GLint size, GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glIndexFormatNV glIndexFormatNV<c-o>:echoh HintHL<Bar>echo "void glIndexFormatNV(GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glTexCoordFormatNV glTexCoordFormatNV<c-o>:echoh HintHL<Bar>echo "void glTexCoordFormatNV(GLint size, GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glEdgeFlagFormatNV glEdgeFlagFormatNV<c-o>:echoh HintHL<Bar>echo "void glEdgeFlagFormatNV(GLsizei stride)"<Bar>echoh None<cr>
inorea glSecondaryColorFormatNV glSecondaryColorFormatNV<c-o>:echoh HintHL<Bar>echo "void glSecondaryColorFormatNV(GLint size, GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glFogCoordFormatNV glFogCoordFormatNV<c-o>:echoh HintHL<Bar>echo "void glFogCoordFormatNV(GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glVertexAttribFormatNV glVertexAttribFormatNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribFormatNV(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride)"<Bar>echoh None<cr>
inorea glVertexAttribIFormatNV glVertexAttribIFormatNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribIFormatNV(GLuint index, GLint size, GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glGetIntegerui64i_vNV glGetIntegerui64i_vNV<c-o>:echoh HintHL<Bar>echo "void glGetIntegerui64i_vNV(GLenum value, GLuint index, GLuint64EXT *result)"<Bar>echoh None<cr>
inorea glTextureBarrierNV glTextureBarrierNV<c-o>:echoh HintHL<Bar>echo "void glTextureBarrierNV(void)"<Bar>echoh None<cr>
inorea glBindImageTextureEXT glBindImageTextureEXT<c-o>:echoh HintHL<Bar>echo "void glBindImageTextureEXT(GLuint index, GLuint texture, GLint level, GLboolean layered, GLint layer, GLenum access, GLint format)"<Bar>echoh None<cr>
inorea glMemoryBarrierEXT glMemoryBarrierEXT<c-o>:echoh HintHL<Bar>echo "void glMemoryBarrierEXT(GLbitfield barriers)"<Bar>echoh None<cr>
inorea glVertexAttribL1dEXT glVertexAttribL1dEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1dEXT(GLuint index, GLdouble x)"<Bar>echoh None<cr>
inorea glVertexAttribL2dEXT glVertexAttribL2dEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2dEXT(GLuint index, GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertexAttribL3dEXT glVertexAttribL3dEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3dEXT(GLuint index, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertexAttribL4dEXT glVertexAttribL4dEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4dEXT(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertexAttribL1dvEXT glVertexAttribL1dvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1dvEXT(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribL2dvEXT glVertexAttribL2dvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2dvEXT(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribL3dvEXT glVertexAttribL3dvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3dvEXT(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribL4dvEXT glVertexAttribL4dvEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4dvEXT(GLuint index, const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertexAttribLPointerEXT glVertexAttribLPointerEXT<c-o>:echoh HintHL<Bar>echo "void glVertexAttribLPointerEXT(GLuint index, GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glGetVertexAttribLdvEXT glGetVertexAttribLdvEXT<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribLdvEXT(GLuint index, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glVertexArrayVertexAttribLOffsetEXT glVertexArrayVertexAttribLOffsetEXT<c-o>:echoh HintHL<Bar>echo "void glVertexArrayVertexAttribLOffsetEXT(GLuint vaobj, GLuint buffer, GLuint index, GLint size, GLenum type, GLsizei stride, GLintptr offset)"<Bar>echoh None<cr>
inorea glProgramSubroutineParametersuivNV glProgramSubroutineParametersuivNV<c-o>:echoh HintHL<Bar>echo "void glProgramSubroutineParametersuivNV(GLenum target, GLsizei count, const GLuint *params)"<Bar>echoh None<cr>
inorea glGetProgramSubroutineParameteruivNV glGetProgramSubroutineParameteruivNV<c-o>:echoh HintHL<Bar>echo "void glGetProgramSubroutineParameteruivNV(GLenum target, GLuint index, GLuint *param)"<Bar>echoh None<cr>
inorea glUniform1i64NV glUniform1i64NV<c-o>:echoh HintHL<Bar>echo "void glUniform1i64NV(GLint location, GLint64EXT x)"<Bar>echoh None<cr>
inorea glUniform2i64NV glUniform2i64NV<c-o>:echoh HintHL<Bar>echo "void glUniform2i64NV(GLint location, GLint64EXT x, GLint64EXT y)"<Bar>echoh None<cr>
inorea glUniform3i64NV glUniform3i64NV<c-o>:echoh HintHL<Bar>echo "void glUniform3i64NV(GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z)"<Bar>echoh None<cr>
inorea glUniform4i64NV glUniform4i64NV<c-o>:echoh HintHL<Bar>echo "void glUniform4i64NV(GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z, GLint64EXT w)"<Bar>echoh None<cr>
inorea glUniform1i64vNV glUniform1i64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform1i64vNV(GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glUniform2i64vNV glUniform2i64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform2i64vNV(GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glUniform3i64vNV glUniform3i64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform3i64vNV(GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glUniform4i64vNV glUniform4i64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform4i64vNV(GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glUniform1ui64NV glUniform1ui64NV<c-o>:echoh HintHL<Bar>echo "void glUniform1ui64NV(GLint location, GLuint64EXT x)"<Bar>echoh None<cr>
inorea glUniform2ui64NV glUniform2ui64NV<c-o>:echoh HintHL<Bar>echo "void glUniform2ui64NV(GLint location, GLuint64EXT x, GLuint64EXT y)"<Bar>echoh None<cr>
inorea glUniform3ui64NV glUniform3ui64NV<c-o>:echoh HintHL<Bar>echo "void glUniform3ui64NV(GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z)"<Bar>echoh None<cr>
inorea glUniform4ui64NV glUniform4ui64NV<c-o>:echoh HintHL<Bar>echo "void glUniform4ui64NV(GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z, GLuint64EXT w)"<Bar>echoh None<cr>
inorea glUniform1ui64vNV glUniform1ui64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform1ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glUniform2ui64vNV glUniform2ui64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform2ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glUniform3ui64vNV glUniform3ui64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform3ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glUniform4ui64vNV glUniform4ui64vNV<c-o>:echoh HintHL<Bar>echo "void glUniform4ui64vNV(GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glGetUniformi64vNV glGetUniformi64vNV<c-o>:echoh HintHL<Bar>echo "void glGetUniformi64vNV(GLuint program, GLint location, GLint64EXT *params)"<Bar>echoh None<cr>
inorea glProgramUniform1i64NV glProgramUniform1i64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1i64NV(GLuint program, GLint location, GLint64EXT x)"<Bar>echoh None<cr>
inorea glProgramUniform2i64NV glProgramUniform2i64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2i64NV(GLuint program, GLint location, GLint64EXT x, GLint64EXT y)"<Bar>echoh None<cr>
inorea glProgramUniform3i64NV glProgramUniform3i64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3i64NV(GLuint program, GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z)"<Bar>echoh None<cr>
inorea glProgramUniform4i64NV glProgramUniform4i64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4i64NV(GLuint program, GLint location, GLint64EXT x, GLint64EXT y, GLint64EXT z, GLint64EXT w)"<Bar>echoh None<cr>
inorea glProgramUniform1i64vNV glProgramUniform1i64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glProgramUniform2i64vNV glProgramUniform2i64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glProgramUniform3i64vNV glProgramUniform3i64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glProgramUniform4i64vNV glProgramUniform4i64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4i64vNV(GLuint program, GLint location, GLsizei count, const GLint64EXT *value)"<Bar>echoh None<cr>
inorea glProgramUniform1ui64NV glProgramUniform1ui64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1ui64NV(GLuint program, GLint location, GLuint64EXT x)"<Bar>echoh None<cr>
inorea glProgramUniform2ui64NV glProgramUniform2ui64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2ui64NV(GLuint program, GLint location, GLuint64EXT x, GLuint64EXT y)"<Bar>echoh None<cr>
inorea glProgramUniform3ui64NV glProgramUniform3ui64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3ui64NV(GLuint program, GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z)"<Bar>echoh None<cr>
inorea glProgramUniform4ui64NV glProgramUniform4ui64NV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4ui64NV(GLuint program, GLint location, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z, GLuint64EXT w)"<Bar>echoh None<cr>
inorea glProgramUniform1ui64vNV glProgramUniform1ui64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform1ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glProgramUniform2ui64vNV glProgramUniform2ui64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform2ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glProgramUniform3ui64vNV glProgramUniform3ui64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform3ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glProgramUniform4ui64vNV glProgramUniform4ui64vNV<c-o>:echoh HintHL<Bar>echo "void glProgramUniform4ui64vNV(GLuint program, GLint location, GLsizei count, const GLuint64EXT *value)"<Bar>echoh None<cr>
inorea glVertexAttribL1i64NV glVertexAttribL1i64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1i64NV(GLuint index, GLint64EXT x)"<Bar>echoh None<cr>
inorea glVertexAttribL2i64NV glVertexAttribL2i64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2i64NV(GLuint index, GLint64EXT x, GLint64EXT y)"<Bar>echoh None<cr>
inorea glVertexAttribL3i64NV glVertexAttribL3i64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3i64NV(GLuint index, GLint64EXT x, GLint64EXT y, GLint64EXT z)"<Bar>echoh None<cr>
inorea glVertexAttribL4i64NV glVertexAttribL4i64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4i64NV(GLuint index, GLint64EXT x, GLint64EXT y, GLint64EXT z, GLint64EXT w)"<Bar>echoh None<cr>
inorea glVertexAttribL1i64vNV glVertexAttribL1i64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1i64vNV(GLuint index, const GLint64EXT *v)"<Bar>echoh None<cr>
inorea glVertexAttribL2i64vNV glVertexAttribL2i64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2i64vNV(GLuint index, const GLint64EXT *v)"<Bar>echoh None<cr>
inorea glVertexAttribL3i64vNV glVertexAttribL3i64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3i64vNV(GLuint index, const GLint64EXT *v)"<Bar>echoh None<cr>
inorea glVertexAttribL4i64vNV glVertexAttribL4i64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4i64vNV(GLuint index, const GLint64EXT *v)"<Bar>echoh None<cr>
inorea glVertexAttribL1ui64NV glVertexAttribL1ui64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1ui64NV(GLuint index, GLuint64EXT x)"<Bar>echoh None<cr>
inorea glVertexAttribL2ui64NV glVertexAttribL2ui64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2ui64NV(GLuint index, GLuint64EXT x, GLuint64EXT y)"<Bar>echoh None<cr>
inorea glVertexAttribL3ui64NV glVertexAttribL3ui64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3ui64NV(GLuint index, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z)"<Bar>echoh None<cr>
inorea glVertexAttribL4ui64NV glVertexAttribL4ui64NV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4ui64NV(GLuint index, GLuint64EXT x, GLuint64EXT y, GLuint64EXT z, GLuint64EXT w)"<Bar>echoh None<cr>
inorea glVertexAttribL1ui64vNV glVertexAttribL1ui64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL1ui64vNV(GLuint index, const GLuint64EXT *v)"<Bar>echoh None<cr>
inorea glVertexAttribL2ui64vNV glVertexAttribL2ui64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL2ui64vNV(GLuint index, const GLuint64EXT *v)"<Bar>echoh None<cr>
inorea glVertexAttribL3ui64vNV glVertexAttribL3ui64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL3ui64vNV(GLuint index, const GLuint64EXT *v)"<Bar>echoh None<cr>
inorea glVertexAttribL4ui64vNV glVertexAttribL4ui64vNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribL4ui64vNV(GLuint index, const GLuint64EXT *v)"<Bar>echoh None<cr>
inorea glGetVertexAttribLi64vNV glGetVertexAttribLi64vNV<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribLi64vNV(GLuint index, GLenum pname, GLint64EXT *params)"<Bar>echoh None<cr>
inorea glGetVertexAttribLui64vNV glGetVertexAttribLui64vNV<c-o>:echoh HintHL<Bar>echo "void glGetVertexAttribLui64vNV(GLuint index, GLenum pname, GLuint64EXT *params)"<Bar>echoh None<cr>
inorea glVertexAttribLFormatNV glVertexAttribLFormatNV<c-o>:echoh HintHL<Bar>echo "void glVertexAttribLFormatNV(GLuint index, GLint size, GLenum type, GLsizei stride)"<Bar>echoh None<cr>
inorea glGenNamesAMD glGenNamesAMD<c-o>:echoh HintHL<Bar>echo "void glGenNamesAMD(GLenum identifier, GLuint num, GLuint *names)"<Bar>echoh None<cr>
inorea glDeleteNamesAMD glDeleteNamesAMD<c-o>:echoh HintHL<Bar>echo "void glDeleteNamesAMD(GLenum identifier, GLuint num, const GLuint *names)"<Bar>echoh None<cr>
inorea glIsNameAMD glIsNameAMD<c-o>:echoh HintHL<Bar>echo "GLboolean glIsNameAMD(GLenum identifier, GLuint name)"<Bar>echoh None<cr>
inorea glDebugMessageEnableAMD glDebugMessageEnableAMD<c-o>:echoh HintHL<Bar>echo "void glDebugMessageEnableAMD(GLenum category, GLenum severity, GLsizei count, const GLuint *ids, GLboolean enabled)"<Bar>echoh None<cr>
inorea glDebugMessageInsertAMD glDebugMessageInsertAMD<c-o>:echoh HintHL<Bar>echo "void glDebugMessageInsertAMD(GLenum category, GLenum severity, GLuint id, GLsizei length, const GLchar *buf)"<Bar>echoh None<cr>
inorea glDebugMessageCallbackAMD glDebugMessageCallbackAMD<c-o>:echoh HintHL<Bar>echo "void glDebugMessageCallbackAMD(GLDEBUGPROCAMD callback, GLvoid *userParam)"<Bar>echoh None<cr>
inorea glGetDebugMessageLogAMD glGetDebugMessageLogAMD<c-o>:echoh HintHL<Bar>echo "GLuint glGetDebugMessageLogAMD(GLuint count, GLsizei bufsize, GLenum *categories, GLuint *severities, GLuint *ids, GLsizei *lengths, GLchar *message)"<Bar>echoh None<cr>
inorea glVDPAUInitNV glVDPAUInitNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUInitNV(const GLvoid *vdpDevice, const GLvoid *getProcAddress)"<Bar>echoh None<cr>
inorea glVDPAUFiniNV glVDPAUFiniNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUFiniNV(void)"<Bar>echoh None<cr>
inorea glVDPAURegisterVideoSurfaceNV glVDPAURegisterVideoSurfaceNV<c-o>:echoh HintHL<Bar>echo "GLvdpauSurfaceNV glVDPAURegisterVideoSurfaceNV(GLvoid *vdpSurface, GLenum target, GLsizei numTextureNames, const GLuint *textureNames)"<Bar>echoh None<cr>
inorea glVDPAURegisterOutputSurfaceNV glVDPAURegisterOutputSurfaceNV<c-o>:echoh HintHL<Bar>echo "GLvdpauSurfaceNV glVDPAURegisterOutputSurfaceNV(GLvoid *vdpSurface, GLenum target, GLsizei numTextureNames, const GLuint *textureNames)"<Bar>echoh None<cr>
inorea glVDPAUIsSurfaceNV glVDPAUIsSurfaceNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUIsSurfaceNV(GLvdpauSurfaceNV surface)"<Bar>echoh None<cr>
inorea glVDPAUUnregisterSurfaceNV glVDPAUUnregisterSurfaceNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUUnregisterSurfaceNV(GLvdpauSurfaceNV surface)"<Bar>echoh None<cr>
inorea glVDPAUGetSurfaceivNV glVDPAUGetSurfaceivNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUGetSurfaceivNV(GLvdpauSurfaceNV surface, GLenum pname, GLsizei bufSize, GLsizei *length, GLint *values)"<Bar>echoh None<cr>
inorea glVDPAUSurfaceAccessNV glVDPAUSurfaceAccessNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUSurfaceAccessNV(GLvdpauSurfaceNV surface, GLenum access)"<Bar>echoh None<cr>
inorea glVDPAUMapSurfacesNV glVDPAUMapSurfacesNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUMapSurfacesNV(GLsizei numSurfaces, const GLvdpauSurfaceNV *surfaces)"<Bar>echoh None<cr>
inorea glVDPAUUnmapSurfacesNV glVDPAUUnmapSurfacesNV<c-o>:echoh HintHL<Bar>echo "void glVDPAUUnmapSurfacesNV(GLsizei numSurface, const GLvdpauSurfaceNV *surfaces)"<Bar>echoh None<cr>
inorea glClearIndex glClearIndex<c-o>:echoh HintHL<Bar>echo "void glClearIndex(GLfloat c)"<Bar>echoh None<cr>
inorea glClearColor glClearColor<c-o>:echoh HintHL<Bar>echo "void glClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"<Bar>echoh None<cr>
inorea glClear glClear<c-o>:echoh HintHL<Bar>echo "void glClear(GLbitfield mask)"<Bar>echoh None<cr>
inorea glIndexMask glIndexMask<c-o>:echoh HintHL<Bar>echo "void glIndexMask(GLuint mask)"<Bar>echoh None<cr>
inorea glColorMask glColorMask<c-o>:echoh HintHL<Bar>echo "void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)"<Bar>echoh None<cr>
inorea glAlphaFunc glAlphaFunc<c-o>:echoh HintHL<Bar>echo "void glAlphaFunc(GLenum func, GLclampf ref)"<Bar>echoh None<cr>
inorea glBlendFunc glBlendFunc<c-o>:echoh HintHL<Bar>echo "void glBlendFunc(GLenum sfactor, GLenum dfactor)"<Bar>echoh None<cr>
inorea glLogicOp glLogicOp<c-o>:echoh HintHL<Bar>echo "void glLogicOp(GLenum opcode)"<Bar>echoh None<cr>
inorea glCullFace glCullFace<c-o>:echoh HintHL<Bar>echo "void glCullFace(GLenum mode)"<Bar>echoh None<cr>
inorea glFrontFace glFrontFace<c-o>:echoh HintHL<Bar>echo "void glFrontFace(GLenum mode)"<Bar>echoh None<cr>
inorea glPointSize glPointSize<c-o>:echoh HintHL<Bar>echo "void glPointSize(GLfloat size)"<Bar>echoh None<cr>
inorea glLineWidth glLineWidth<c-o>:echoh HintHL<Bar>echo "void glLineWidth(GLfloat width)"<Bar>echoh None<cr>
inorea glLineStipple glLineStipple<c-o>:echoh HintHL<Bar>echo "void glLineStipple(GLint factor, GLushort pattern)"<Bar>echoh None<cr>
inorea glPolygonMode glPolygonMode<c-o>:echoh HintHL<Bar>echo "void glPolygonMode(GLenum face, GLenum mode)"<Bar>echoh None<cr>
inorea glPolygonOffset glPolygonOffset<c-o>:echoh HintHL<Bar>echo "void glPolygonOffset(GLfloat factor, GLfloat units)"<Bar>echoh None<cr>
inorea glPolygonStipple glPolygonStipple<c-o>:echoh HintHL<Bar>echo "void glPolygonStipple(const GLubyte *mask)"<Bar>echoh None<cr>
inorea glGetPolygonStipple glGetPolygonStipple<c-o>:echoh HintHL<Bar>echo "void glGetPolygonStipple(GLubyte *mask)"<Bar>echoh None<cr>
inorea glEdgeFlag glEdgeFlag<c-o>:echoh HintHL<Bar>echo "void glEdgeFlag(GLboolean flag)"<Bar>echoh None<cr>
inorea glEdgeFlagv glEdgeFlagv<c-o>:echoh HintHL<Bar>echo "void glEdgeFlagv(const GLboolean *flag)"<Bar>echoh None<cr>
inorea glScissor glScissor<c-o>:echoh HintHL<Bar>echo "void glScissor(GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glClipPlane glClipPlane<c-o>:echoh HintHL<Bar>echo "void glClipPlane(GLenum plane, const GLdouble *equation)"<Bar>echoh None<cr>
inorea glGetClipPlane glGetClipPlane<c-o>:echoh HintHL<Bar>echo "void glGetClipPlane(GLenum plane, GLdouble *equation)"<Bar>echoh None<cr>
inorea glDrawBuffer glDrawBuffer<c-o>:echoh HintHL<Bar>echo "void glDrawBuffer(GLenum mode)"<Bar>echoh None<cr>
inorea glReadBuffer glReadBuffer<c-o>:echoh HintHL<Bar>echo "void glReadBuffer(GLenum mode)"<Bar>echoh None<cr>
inorea glEnable glEnable<c-o>:echoh HintHL<Bar>echo "void glEnable(GLenum cap)"<Bar>echoh None<cr>
inorea glDisable glDisable<c-o>:echoh HintHL<Bar>echo "void glDisable(GLenum cap)"<Bar>echoh None<cr>
inorea glIsEnabled glIsEnabled<c-o>:echoh HintHL<Bar>echo "GLboolean glIsEnabled(GLenum cap)"<Bar>echoh None<cr>
inorea glEnableClientState glEnableClientState<c-o>:echoh HintHL<Bar>echo "void glEnableClientState(GLenum cap)"<Bar>echoh None<cr>
inorea glDisableClientState glDisableClientState<c-o>:echoh HintHL<Bar>echo "void glDisableClientState(GLenum cap)"<Bar>echoh None<cr>
inorea glGetBooleanv glGetBooleanv<c-o>:echoh HintHL<Bar>echo "void glGetBooleanv(GLenum pname, GLboolean *params)"<Bar>echoh None<cr>
inorea glGetDoublev glGetDoublev<c-o>:echoh HintHL<Bar>echo "void glGetDoublev(GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetFloatv glGetFloatv<c-o>:echoh HintHL<Bar>echo "void glGetFloatv(GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetIntegerv glGetIntegerv<c-o>:echoh HintHL<Bar>echo "void glGetIntegerv(GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glPushAttrib glPushAttrib<c-o>:echoh HintHL<Bar>echo "void glPushAttrib(GLbitfield mask)"<Bar>echoh None<cr>
inorea glPopAttrib glPopAttrib<c-o>:echoh HintHL<Bar>echo "void glPopAttrib(void)"<Bar>echoh None<cr>
inorea glPushClientAttrib glPushClientAttrib<c-o>:echoh HintHL<Bar>echo "void glPushClientAttrib(GLbitfield mask)"<Bar>echoh None<cr>
inorea glPopClientAttrib glPopClientAttrib<c-o>:echoh HintHL<Bar>echo "void glPopClientAttrib(void)"<Bar>echoh None<cr>
inorea glRenderMode glRenderMode<c-o>:echoh HintHL<Bar>echo "GLint glRenderMode(GLenum mode)"<Bar>echoh None<cr>
inorea glGetError glGetError<c-o>:echoh HintHL<Bar>echo "GLenum glGetError(void)"<Bar>echoh None<cr>
inorea glGetString glGetString<c-o>:echoh HintHL<Bar>echo "const GLubyte *glGetString(GLenum name)"<Bar>echoh None<cr>
inorea glFinish glFinish<c-o>:echoh HintHL<Bar>echo "void glFinish(void)"<Bar>echoh None<cr>
inorea glFlush glFlush<c-o>:echoh HintHL<Bar>echo "void glFlush(void)"<Bar>echoh None<cr>
inorea glHint glHint<c-o>:echoh HintHL<Bar>echo "void glHint(GLenum target, GLenum mode)"<Bar>echoh None<cr>
inorea glClearDepth glClearDepth<c-o>:echoh HintHL<Bar>echo "void glClearDepth(GLclampd depth)"<Bar>echoh None<cr>
inorea glDepthFunc glDepthFunc<c-o>:echoh HintHL<Bar>echo "void glDepthFunc(GLenum func)"<Bar>echoh None<cr>
inorea glDepthMask glDepthMask<c-o>:echoh HintHL<Bar>echo "void glDepthMask(GLboolean flag)"<Bar>echoh None<cr>
inorea glDepthRange glDepthRange<c-o>:echoh HintHL<Bar>echo "void glDepthRange(GLclampd near_val, GLclampd far_val)"<Bar>echoh None<cr>
inorea glClearAccum glClearAccum<c-o>:echoh HintHL<Bar>echo "void glClearAccum(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"<Bar>echoh None<cr>
inorea glAccum glAccum<c-o>:echoh HintHL<Bar>echo "void glAccum(GLenum op, GLfloat value)"<Bar>echoh None<cr>
inorea glMatrixMode glMatrixMode<c-o>:echoh HintHL<Bar>echo "void glMatrixMode(GLenum mode)"<Bar>echoh None<cr>
inorea glOrtho glOrtho<c-o>:echoh HintHL<Bar>echo "void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble nearVal, GLdouble farVal)"<Bar>echoh None<cr>
inorea glFrustum glFrustum<c-o>:echoh HintHL<Bar>echo "void glFrustum(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble nearVal, GLdouble farVal)"<Bar>echoh None<cr>
inorea glViewport glViewport<c-o>:echoh HintHL<Bar>echo "void glViewport(GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glPushMatrix glPushMatrix<c-o>:echoh HintHL<Bar>echo "void glPushMatrix(void)"<Bar>echoh None<cr>
inorea glPopMatrix glPopMatrix<c-o>:echoh HintHL<Bar>echo "void glPopMatrix(void)"<Bar>echoh None<cr>
inorea glLoadIdentity glLoadIdentity<c-o>:echoh HintHL<Bar>echo "void glLoadIdentity(void)"<Bar>echoh None<cr>
inorea glLoadMatrixd glLoadMatrixd<c-o>:echoh HintHL<Bar>echo "void glLoadMatrixd(const GLdouble *m)"<Bar>echoh None<cr>
inorea glLoadMatrixf glLoadMatrixf<c-o>:echoh HintHL<Bar>echo "void glLoadMatrixf(const GLfloat *m)"<Bar>echoh None<cr>
inorea glMultMatrixd glMultMatrixd<c-o>:echoh HintHL<Bar>echo "void glMultMatrixd(const GLdouble *m)"<Bar>echoh None<cr>
inorea glMultMatrixf glMultMatrixf<c-o>:echoh HintHL<Bar>echo "void glMultMatrixf(const GLfloat *m)"<Bar>echoh None<cr>
inorea glRotated glRotated<c-o>:echoh HintHL<Bar>echo "void glRotated(GLdouble angle, GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glRotatef glRotatef<c-o>:echoh HintHL<Bar>echo "void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glScaled glScaled<c-o>:echoh HintHL<Bar>echo "void glScaled(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glScalef glScalef<c-o>:echoh HintHL<Bar>echo "void glScalef(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glTranslated glTranslated<c-o>:echoh HintHL<Bar>echo "void glTranslated(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glTranslatef glTranslatef<c-o>:echoh HintHL<Bar>echo "void glTranslatef(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glIsList glIsList<c-o>:echoh HintHL<Bar>echo "GLboolean glIsList(GLuint list)"<Bar>echoh None<cr>
inorea glDeleteLists glDeleteLists<c-o>:echoh HintHL<Bar>echo "void glDeleteLists(GLuint list, GLsizei range)"<Bar>echoh None<cr>
inorea glGenLists glGenLists<c-o>:echoh HintHL<Bar>echo "GLuint glGenLists(GLsizei range)"<Bar>echoh None<cr>
inorea glNewList glNewList<c-o>:echoh HintHL<Bar>echo "void glNewList(GLuint list, GLenum mode)"<Bar>echoh None<cr>
inorea glEndList glEndList<c-o>:echoh HintHL<Bar>echo "void glEndList(void)"<Bar>echoh None<cr>
inorea glCallList glCallList<c-o>:echoh HintHL<Bar>echo "void glCallList(GLuint list)"<Bar>echoh None<cr>
inorea glCallLists glCallLists<c-o>:echoh HintHL<Bar>echo "void glCallLists(GLsizei n, GLenum type, const GLvoid *lists)"<Bar>echoh None<cr>
inorea glListBase glListBase<c-o>:echoh HintHL<Bar>echo "void glListBase(GLuint base)"<Bar>echoh None<cr>
inorea glBegin glBegin<c-o>:echoh HintHL<Bar>echo "void glBegin(GLenum mode)"<Bar>echoh None<cr>
inorea glEnd glEnd<c-o>:echoh HintHL<Bar>echo "void glEnd(void)"<Bar>echoh None<cr>
inorea glVertex2d glVertex2d<c-o>:echoh HintHL<Bar>echo "void glVertex2d(GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glVertex2f glVertex2f<c-o>:echoh HintHL<Bar>echo "void glVertex2f(GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glVertex2i glVertex2i<c-o>:echoh HintHL<Bar>echo "void glVertex2i(GLint x, GLint y)"<Bar>echoh None<cr>
inorea glVertex2s glVertex2s<c-o>:echoh HintHL<Bar>echo "void glVertex2s(GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glVertex3d glVertex3d<c-o>:echoh HintHL<Bar>echo "void glVertex3d(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glVertex3f glVertex3f<c-o>:echoh HintHL<Bar>echo "void glVertex3f(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glVertex3i glVertex3i<c-o>:echoh HintHL<Bar>echo "void glVertex3i(GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glVertex3s glVertex3s<c-o>:echoh HintHL<Bar>echo "void glVertex3s(GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glVertex4d glVertex4d<c-o>:echoh HintHL<Bar>echo "void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glVertex4f glVertex4f<c-o>:echoh HintHL<Bar>echo "void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glVertex4i glVertex4i<c-o>:echoh HintHL<Bar>echo "void glVertex4i(GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glVertex4s glVertex4s<c-o>:echoh HintHL<Bar>echo "void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glVertex2dv glVertex2dv<c-o>:echoh HintHL<Bar>echo "void glVertex2dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertex2fv glVertex2fv<c-o>:echoh HintHL<Bar>echo "void glVertex2fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertex2iv glVertex2iv<c-o>:echoh HintHL<Bar>echo "void glVertex2iv(const GLint *v)"<Bar>echoh None<cr>
inorea glVertex2sv glVertex2sv<c-o>:echoh HintHL<Bar>echo "void glVertex2sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glVertex3dv glVertex3dv<c-o>:echoh HintHL<Bar>echo "void glVertex3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertex3fv glVertex3fv<c-o>:echoh HintHL<Bar>echo "void glVertex3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertex3iv glVertex3iv<c-o>:echoh HintHL<Bar>echo "void glVertex3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glVertex3sv glVertex3sv<c-o>:echoh HintHL<Bar>echo "void glVertex3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glVertex4dv glVertex4dv<c-o>:echoh HintHL<Bar>echo "void glVertex4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glVertex4fv glVertex4fv<c-o>:echoh HintHL<Bar>echo "void glVertex4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glVertex4iv glVertex4iv<c-o>:echoh HintHL<Bar>echo "void glVertex4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glVertex4sv glVertex4sv<c-o>:echoh HintHL<Bar>echo "void glVertex4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glNormal3b glNormal3b<c-o>:echoh HintHL<Bar>echo "void glNormal3b(GLbyte nx, GLbyte ny, GLbyte nz)"<Bar>echoh None<cr>
inorea glNormal3d glNormal3d<c-o>:echoh HintHL<Bar>echo "void glNormal3d(GLdouble nx, GLdouble ny, GLdouble nz)"<Bar>echoh None<cr>
inorea glNormal3f glNormal3f<c-o>:echoh HintHL<Bar>echo "void glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz)"<Bar>echoh None<cr>
inorea glNormal3i glNormal3i<c-o>:echoh HintHL<Bar>echo "void glNormal3i(GLint nx, GLint ny, GLint nz)"<Bar>echoh None<cr>
inorea glNormal3s glNormal3s<c-o>:echoh HintHL<Bar>echo "void glNormal3s(GLshort nx, GLshort ny, GLshort nz)"<Bar>echoh None<cr>
inorea glNormal3bv glNormal3bv<c-o>:echoh HintHL<Bar>echo "void glNormal3bv(const GLbyte *v)"<Bar>echoh None<cr>
inorea glNormal3dv glNormal3dv<c-o>:echoh HintHL<Bar>echo "void glNormal3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glNormal3fv glNormal3fv<c-o>:echoh HintHL<Bar>echo "void glNormal3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glNormal3iv glNormal3iv<c-o>:echoh HintHL<Bar>echo "void glNormal3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glNormal3sv glNormal3sv<c-o>:echoh HintHL<Bar>echo "void glNormal3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glIndexd glIndexd<c-o>:echoh HintHL<Bar>echo "void glIndexd(GLdouble c)"<Bar>echoh None<cr>
inorea glIndexf glIndexf<c-o>:echoh HintHL<Bar>echo "void glIndexf(GLfloat c)"<Bar>echoh None<cr>
inorea glIndexi glIndexi<c-o>:echoh HintHL<Bar>echo "void glIndexi(GLint c)"<Bar>echoh None<cr>
inorea glIndexs glIndexs<c-o>:echoh HintHL<Bar>echo "void glIndexs(GLshort c)"<Bar>echoh None<cr>
inorea glIndexub glIndexub<c-o>:echoh HintHL<Bar>echo "void glIndexub(GLubyte c)"<Bar>echoh None<cr>
inorea glIndexdv glIndexdv<c-o>:echoh HintHL<Bar>echo "void glIndexdv(const GLdouble *c)"<Bar>echoh None<cr>
inorea glIndexfv glIndexfv<c-o>:echoh HintHL<Bar>echo "void glIndexfv(const GLfloat *c)"<Bar>echoh None<cr>
inorea glIndexiv glIndexiv<c-o>:echoh HintHL<Bar>echo "void glIndexiv(const GLint *c)"<Bar>echoh None<cr>
inorea glIndexsv glIndexsv<c-o>:echoh HintHL<Bar>echo "void glIndexsv(const GLshort *c)"<Bar>echoh None<cr>
inorea glIndexubv glIndexubv<c-o>:echoh HintHL<Bar>echo "void glIndexubv(const GLubyte *c)"<Bar>echoh None<cr>
inorea glColor3b glColor3b<c-o>:echoh HintHL<Bar>echo "void glColor3b(GLbyte red, GLbyte green, GLbyte blue)"<Bar>echoh None<cr>
inorea glColor3d glColor3d<c-o>:echoh HintHL<Bar>echo "void glColor3d(GLdouble red, GLdouble green, GLdouble blue)"<Bar>echoh None<cr>
inorea glColor3f glColor3f<c-o>:echoh HintHL<Bar>echo "void glColor3f(GLfloat red, GLfloat green, GLfloat blue)"<Bar>echoh None<cr>
inorea glColor3i glColor3i<c-o>:echoh HintHL<Bar>echo "void glColor3i(GLint red, GLint green, GLint blue)"<Bar>echoh None<cr>
inorea glColor3s glColor3s<c-o>:echoh HintHL<Bar>echo "void glColor3s(GLshort red, GLshort green, GLshort blue)"<Bar>echoh None<cr>
inorea glColor3ub glColor3ub<c-o>:echoh HintHL<Bar>echo "void glColor3ub(GLubyte red, GLubyte green, GLubyte blue)"<Bar>echoh None<cr>
inorea glColor3ui glColor3ui<c-o>:echoh HintHL<Bar>echo "void glColor3ui(GLuint red, GLuint green, GLuint blue)"<Bar>echoh None<cr>
inorea glColor3us glColor3us<c-o>:echoh HintHL<Bar>echo "void glColor3us(GLushort red, GLushort green, GLushort blue)"<Bar>echoh None<cr>
inorea glColor4b glColor4b<c-o>:echoh HintHL<Bar>echo "void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha)"<Bar>echoh None<cr>
inorea glColor4d glColor4d<c-o>:echoh HintHL<Bar>echo "void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)"<Bar>echoh None<cr>
inorea glColor4f glColor4f<c-o>:echoh HintHL<Bar>echo "void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)"<Bar>echoh None<cr>
inorea glColor4i glColor4i<c-o>:echoh HintHL<Bar>echo "void glColor4i(GLint red, GLint green, GLint blue, GLint alpha)"<Bar>echoh None<cr>
inorea glColor4s glColor4s<c-o>:echoh HintHL<Bar>echo "void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha)"<Bar>echoh None<cr>
inorea glColor4ub glColor4ub<c-o>:echoh HintHL<Bar>echo "void glColor4ub(GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha)"<Bar>echoh None<cr>
inorea glColor4ui glColor4ui<c-o>:echoh HintHL<Bar>echo "void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha)"<Bar>echoh None<cr>
inorea glColor4us glColor4us<c-o>:echoh HintHL<Bar>echo "void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha)"<Bar>echoh None<cr>
inorea glColor3bv glColor3bv<c-o>:echoh HintHL<Bar>echo "void glColor3bv(const GLbyte *v)"<Bar>echoh None<cr>
inorea glColor3dv glColor3dv<c-o>:echoh HintHL<Bar>echo "void glColor3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glColor3fv glColor3fv<c-o>:echoh HintHL<Bar>echo "void glColor3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glColor3iv glColor3iv<c-o>:echoh HintHL<Bar>echo "void glColor3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glColor3sv glColor3sv<c-o>:echoh HintHL<Bar>echo "void glColor3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glColor3ubv glColor3ubv<c-o>:echoh HintHL<Bar>echo "void glColor3ubv(const GLubyte *v)"<Bar>echoh None<cr>
inorea glColor3uiv glColor3uiv<c-o>:echoh HintHL<Bar>echo "void glColor3uiv(const GLuint *v)"<Bar>echoh None<cr>
inorea glColor3usv glColor3usv<c-o>:echoh HintHL<Bar>echo "void glColor3usv(const GLushort *v)"<Bar>echoh None<cr>
inorea glColor4bv glColor4bv<c-o>:echoh HintHL<Bar>echo "void glColor4bv(const GLbyte *v)"<Bar>echoh None<cr>
inorea glColor4dv glColor4dv<c-o>:echoh HintHL<Bar>echo "void glColor4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glColor4fv glColor4fv<c-o>:echoh HintHL<Bar>echo "void glColor4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glColor4iv glColor4iv<c-o>:echoh HintHL<Bar>echo "void glColor4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glColor4sv glColor4sv<c-o>:echoh HintHL<Bar>echo "void glColor4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glColor4ubv glColor4ubv<c-o>:echoh HintHL<Bar>echo "void glColor4ubv(const GLubyte *v)"<Bar>echoh None<cr>
inorea glColor4uiv glColor4uiv<c-o>:echoh HintHL<Bar>echo "void glColor4uiv(const GLuint *v)"<Bar>echoh None<cr>
inorea glColor4usv glColor4usv<c-o>:echoh HintHL<Bar>echo "void glColor4usv(const GLushort *v)"<Bar>echoh None<cr>
inorea glTexCoord1d glTexCoord1d<c-o>:echoh HintHL<Bar>echo "void glTexCoord1d(GLdouble s)"<Bar>echoh None<cr>
inorea glTexCoord1f glTexCoord1f<c-o>:echoh HintHL<Bar>echo "void glTexCoord1f(GLfloat s)"<Bar>echoh None<cr>
inorea glTexCoord1i glTexCoord1i<c-o>:echoh HintHL<Bar>echo "void glTexCoord1i(GLint s)"<Bar>echoh None<cr>
inorea glTexCoord1s glTexCoord1s<c-o>:echoh HintHL<Bar>echo "void glTexCoord1s(GLshort s)"<Bar>echoh None<cr>
inorea glTexCoord2d glTexCoord2d<c-o>:echoh HintHL<Bar>echo "void glTexCoord2d(GLdouble s, GLdouble t)"<Bar>echoh None<cr>
inorea glTexCoord2f glTexCoord2f<c-o>:echoh HintHL<Bar>echo "void glTexCoord2f(GLfloat s, GLfloat t)"<Bar>echoh None<cr>
inorea glTexCoord2i glTexCoord2i<c-o>:echoh HintHL<Bar>echo "void glTexCoord2i(GLint s, GLint t)"<Bar>echoh None<cr>
inorea glTexCoord2s glTexCoord2s<c-o>:echoh HintHL<Bar>echo "void glTexCoord2s(GLshort s, GLshort t)"<Bar>echoh None<cr>
inorea glTexCoord3d glTexCoord3d<c-o>:echoh HintHL<Bar>echo "void glTexCoord3d(GLdouble s, GLdouble t, GLdouble r)"<Bar>echoh None<cr>
inorea glTexCoord3f glTexCoord3f<c-o>:echoh HintHL<Bar>echo "void glTexCoord3f(GLfloat s, GLfloat t, GLfloat r)"<Bar>echoh None<cr>
inorea glTexCoord3i glTexCoord3i<c-o>:echoh HintHL<Bar>echo "void glTexCoord3i(GLint s, GLint t, GLint r)"<Bar>echoh None<cr>
inorea glTexCoord3s glTexCoord3s<c-o>:echoh HintHL<Bar>echo "void glTexCoord3s(GLshort s, GLshort t, GLshort r)"<Bar>echoh None<cr>
inorea glTexCoord4d glTexCoord4d<c-o>:echoh HintHL<Bar>echo "void glTexCoord4d(GLdouble s, GLdouble t, GLdouble r, GLdouble q)"<Bar>echoh None<cr>
inorea glTexCoord4f glTexCoord4f<c-o>:echoh HintHL<Bar>echo "void glTexCoord4f(GLfloat s, GLfloat t, GLfloat r, GLfloat q)"<Bar>echoh None<cr>
inorea glTexCoord4i glTexCoord4i<c-o>:echoh HintHL<Bar>echo "void glTexCoord4i(GLint s, GLint t, GLint r, GLint q)"<Bar>echoh None<cr>
inorea glTexCoord4s glTexCoord4s<c-o>:echoh HintHL<Bar>echo "void glTexCoord4s(GLshort s, GLshort t, GLshort r, GLshort q)"<Bar>echoh None<cr>
inorea glTexCoord1dv glTexCoord1dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord1fv glTexCoord1fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord1iv glTexCoord1iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord1sv glTexCoord1sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord1sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glTexCoord2dv glTexCoord2dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord2fv glTexCoord2fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord2iv glTexCoord2iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord2sv glTexCoord2sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord2sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glTexCoord3dv glTexCoord3dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord3fv glTexCoord3fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord3iv glTexCoord3iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord3sv glTexCoord3sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glTexCoord4dv glTexCoord4dv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glTexCoord4fv glTexCoord4fv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glTexCoord4iv glTexCoord4iv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glTexCoord4sv glTexCoord4sv<c-o>:echoh HintHL<Bar>echo "void glTexCoord4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glRasterPos2d glRasterPos2d<c-o>:echoh HintHL<Bar>echo "void glRasterPos2d(GLdouble x, GLdouble y)"<Bar>echoh None<cr>
inorea glRasterPos2f glRasterPos2f<c-o>:echoh HintHL<Bar>echo "void glRasterPos2f(GLfloat x, GLfloat y)"<Bar>echoh None<cr>
inorea glRasterPos2i glRasterPos2i<c-o>:echoh HintHL<Bar>echo "void glRasterPos2i(GLint x, GLint y)"<Bar>echoh None<cr>
inorea glRasterPos2s glRasterPos2s<c-o>:echoh HintHL<Bar>echo "void glRasterPos2s(GLshort x, GLshort y)"<Bar>echoh None<cr>
inorea glRasterPos3d glRasterPos3d<c-o>:echoh HintHL<Bar>echo "void glRasterPos3d(GLdouble x, GLdouble y, GLdouble z)"<Bar>echoh None<cr>
inorea glRasterPos3f glRasterPos3f<c-o>:echoh HintHL<Bar>echo "void glRasterPos3f(GLfloat x, GLfloat y, GLfloat z)"<Bar>echoh None<cr>
inorea glRasterPos3i glRasterPos3i<c-o>:echoh HintHL<Bar>echo "void glRasterPos3i(GLint x, GLint y, GLint z)"<Bar>echoh None<cr>
inorea glRasterPos3s glRasterPos3s<c-o>:echoh HintHL<Bar>echo "void glRasterPos3s(GLshort x, GLshort y, GLshort z)"<Bar>echoh None<cr>
inorea glRasterPos4d glRasterPos4d<c-o>:echoh HintHL<Bar>echo "void glRasterPos4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w)"<Bar>echoh None<cr>
inorea glRasterPos4f glRasterPos4f<c-o>:echoh HintHL<Bar>echo "void glRasterPos4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w)"<Bar>echoh None<cr>
inorea glRasterPos4i glRasterPos4i<c-o>:echoh HintHL<Bar>echo "void glRasterPos4i(GLint x, GLint y, GLint z, GLint w)"<Bar>echoh None<cr>
inorea glRasterPos4s glRasterPos4s<c-o>:echoh HintHL<Bar>echo "void glRasterPos4s(GLshort x, GLshort y, GLshort z, GLshort w)"<Bar>echoh None<cr>
inorea glRasterPos2dv glRasterPos2dv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glRasterPos2fv glRasterPos2fv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glRasterPos2iv glRasterPos2iv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2iv(const GLint *v)"<Bar>echoh None<cr>
inorea glRasterPos2sv glRasterPos2sv<c-o>:echoh HintHL<Bar>echo "void glRasterPos2sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glRasterPos3dv glRasterPos3dv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glRasterPos3fv glRasterPos3fv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glRasterPos3iv glRasterPos3iv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3iv(const GLint *v)"<Bar>echoh None<cr>
inorea glRasterPos3sv glRasterPos3sv<c-o>:echoh HintHL<Bar>echo "void glRasterPos3sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glRasterPos4dv glRasterPos4dv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4dv(const GLdouble *v)"<Bar>echoh None<cr>
inorea glRasterPos4fv glRasterPos4fv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4fv(const GLfloat *v)"<Bar>echoh None<cr>
inorea glRasterPos4iv glRasterPos4iv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4iv(const GLint *v)"<Bar>echoh None<cr>
inorea glRasterPos4sv glRasterPos4sv<c-o>:echoh HintHL<Bar>echo "void glRasterPos4sv(const GLshort *v)"<Bar>echoh None<cr>
inorea glRectd glRectd<c-o>:echoh HintHL<Bar>echo "void glRectd(GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2)"<Bar>echoh None<cr>
inorea glRectf glRectf<c-o>:echoh HintHL<Bar>echo "void glRectf(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2)"<Bar>echoh None<cr>
inorea glRecti glRecti<c-o>:echoh HintHL<Bar>echo "void glRecti(GLint x1, GLint y1, GLint x2, GLint y2)"<Bar>echoh None<cr>
inorea glRects glRects<c-o>:echoh HintHL<Bar>echo "void glRects(GLshort x1, GLshort y1, GLshort x2, GLshort y2)"<Bar>echoh None<cr>
inorea glRectdv glRectdv<c-o>:echoh HintHL<Bar>echo "void glRectdv(const GLdouble *v1, const GLdouble *v2)"<Bar>echoh None<cr>
inorea glRectfv glRectfv<c-o>:echoh HintHL<Bar>echo "void glRectfv(const GLfloat *v1, const GLfloat *v2)"<Bar>echoh None<cr>
inorea glRectiv glRectiv<c-o>:echoh HintHL<Bar>echo "void glRectiv(const GLint *v1, const GLint *v2)"<Bar>echoh None<cr>
inorea glRectsv glRectsv<c-o>:echoh HintHL<Bar>echo "void glRectsv(const GLshort *v1, const GLshort *v2)"<Bar>echoh None<cr>
inorea glVertexPointer glVertexPointer<c-o>:echoh HintHL<Bar>echo "void glVertexPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glNormalPointer glNormalPointer<c-o>:echoh HintHL<Bar>echo "void glNormalPointer(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glColorPointer glColorPointer<c-o>:echoh HintHL<Bar>echo "void glColorPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glIndexPointer glIndexPointer<c-o>:echoh HintHL<Bar>echo "void glIndexPointer(GLenum type, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glTexCoordPointer glTexCoordPointer<c-o>:echoh HintHL<Bar>echo "void glTexCoordPointer(GLint size, GLenum type, GLsizei  stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glEdgeFlagPointer glEdgeFlagPointer<c-o>:echoh HintHL<Bar>echo "void glEdgeFlagPointer(GLsizei stride, const GLvoid *ptr)"<Bar>echoh None<cr>
inorea glGetPointerv glGetPointerv<c-o>:echoh HintHL<Bar>echo "void glGetPointerv(GLenum pname, GLvoid **params)"<Bar>echoh None<cr>
inorea glArrayElement glArrayElement<c-o>:echoh HintHL<Bar>echo "void glArrayElement(GLint i)"<Bar>echoh None<cr>
inorea glDrawArrays glDrawArrays<c-o>:echoh HintHL<Bar>echo "void glDrawArrays(GLenum mode, GLint first, GLsizei count)"<Bar>echoh None<cr>
inorea glDrawElements glDrawElements<c-o>:echoh HintHL<Bar>echo "void glDrawElements(GLenum mode, GLsizei count, GLenum  type, const GLvoid *indices)"<Bar>echoh None<cr>
inorea glInterleavedArrays glInterleavedArrays<c-o>:echoh HintHL<Bar>echo "void glInterleavedArrays(GLenum format, GLsizei stride, const GLvoid *pointer)"<Bar>echoh None<cr>
inorea glShadeModel glShadeModel<c-o>:echoh HintHL<Bar>echo "void glShadeModel(GLenum mode)"<Bar>echoh None<cr>
inorea glLightf glLightf<c-o>:echoh HintHL<Bar>echo "void glLightf(GLenum light, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glLighti glLighti<c-o>:echoh HintHL<Bar>echo "void glLighti(GLenum light, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glLightfv glLightfv<c-o>:echoh HintHL<Bar>echo "void glLightfv(GLenum light, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glLightiv glLightiv<c-o>:echoh HintHL<Bar>echo "void glLightiv(GLenum light, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glGetLightfv glGetLightfv<c-o>:echoh HintHL<Bar>echo "void glGetLightfv(GLenum light, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetLightiv glGetLightiv<c-o>:echoh HintHL<Bar>echo "void glGetLightiv(GLenum light, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glLightModelf glLightModelf<c-o>:echoh HintHL<Bar>echo "void glLightModelf(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glLightModeli glLightModeli<c-o>:echoh HintHL<Bar>echo "void glLightModeli(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glLightModelfv glLightModelfv<c-o>:echoh HintHL<Bar>echo "void glLightModelfv(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glLightModeliv glLightModeliv<c-o>:echoh HintHL<Bar>echo "void glLightModeliv(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glMaterialf glMaterialf<c-o>:echoh HintHL<Bar>echo "void glMaterialf(GLenum face, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glMateriali glMateriali<c-o>:echoh HintHL<Bar>echo "void glMateriali(GLenum face, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glMaterialfv glMaterialfv<c-o>:echoh HintHL<Bar>echo "void glMaterialfv(GLenum face, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glMaterialiv glMaterialiv<c-o>:echoh HintHL<Bar>echo "void glMaterialiv(GLenum face, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glGetMaterialfv glGetMaterialfv<c-o>:echoh HintHL<Bar>echo "void glGetMaterialfv(GLenum face, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMaterialiv glGetMaterialiv<c-o>:echoh HintHL<Bar>echo "void glGetMaterialiv(GLenum face, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glColorMaterial glColorMaterial<c-o>:echoh HintHL<Bar>echo "void glColorMaterial(GLenum face, GLenum mode)"<Bar>echoh None<cr>
inorea glPixelZoom glPixelZoom<c-o>:echoh HintHL<Bar>echo "void glPixelZoom(GLfloat xfactor, GLfloat yfactor)"<Bar>echoh None<cr>
inorea glPixelStoref glPixelStoref<c-o>:echoh HintHL<Bar>echo "void glPixelStoref(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPixelStorei glPixelStorei<c-o>:echoh HintHL<Bar>echo "void glPixelStorei(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPixelTransferf glPixelTransferf<c-o>:echoh HintHL<Bar>echo "void glPixelTransferf(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glPixelTransferi glPixelTransferi<c-o>:echoh HintHL<Bar>echo "void glPixelTransferi(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glPixelMapfv glPixelMapfv<c-o>:echoh HintHL<Bar>echo "void glPixelMapfv(GLenum map, GLsizei mapsize, const GLfloat *values)"<Bar>echoh None<cr>
inorea glPixelMapuiv glPixelMapuiv<c-o>:echoh HintHL<Bar>echo "void glPixelMapuiv(GLenum map, GLsizei mapsize, const GLuint *values)"<Bar>echoh None<cr>
inorea glPixelMapusv glPixelMapusv<c-o>:echoh HintHL<Bar>echo "void glPixelMapusv(GLenum map, GLsizei mapsize, const GLushort *values)"<Bar>echoh None<cr>
inorea glGetPixelMapfv glGetPixelMapfv<c-o>:echoh HintHL<Bar>echo "void glGetPixelMapfv(GLenum map, GLfloat *values)"<Bar>echoh None<cr>
inorea glGetPixelMapuiv glGetPixelMapuiv<c-o>:echoh HintHL<Bar>echo "void glGetPixelMapuiv(GLenum map, GLuint *values)"<Bar>echoh None<cr>
inorea glGetPixelMapusv glGetPixelMapusv<c-o>:echoh HintHL<Bar>echo "void glGetPixelMapusv(GLenum map, GLushort *values)"<Bar>echoh None<cr>
inorea glBitmap glBitmap<c-o>:echoh HintHL<Bar>echo "void glBitmap(GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap)"<Bar>echoh None<cr>
inorea glReadPixels glReadPixels<c-o>:echoh HintHL<Bar>echo "void glReadPixels(GLint x, GLint y, GLsizei  width, GLsizei height, GLenum format, GLenum type, GLvoid *data)"<Bar>echoh None<cr>
inorea glDrawPixels glDrawPixels<c-o>:echoh HintHL<Bar>echo "void glDrawPixels(GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCopyPixels glCopyPixels<c-o>:echoh HintHL<Bar>echo "void glCopyPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum type)"<Bar>echoh None<cr>
inorea glStencilFunc glStencilFunc<c-o>:echoh HintHL<Bar>echo "void glStencilFunc(GLenum func, GLint ref, GLuint mask)"<Bar>echoh None<cr>
inorea glStencilMask glStencilMask<c-o>:echoh HintHL<Bar>echo "void glStencilMask(GLuint mask)"<Bar>echoh None<cr>
inorea glStencilOp glStencilOp<c-o>:echoh HintHL<Bar>echo "void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass)"<Bar>echoh None<cr>
inorea glClearStencil glClearStencil<c-o>:echoh HintHL<Bar>echo "void glClearStencil(GLint s)"<Bar>echoh None<cr>
inorea glTexGend glTexGend<c-o>:echoh HintHL<Bar>echo "void glTexGend(GLenum coord, GLenum pname, GLdouble param)"<Bar>echoh None<cr>
inorea glTexGenf glTexGenf<c-o>:echoh HintHL<Bar>echo "void glTexGenf(GLenum coord, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glTexGeni glTexGeni<c-o>:echoh HintHL<Bar>echo "void glTexGeni(GLenum coord, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glTexGendv glTexGendv<c-o>:echoh HintHL<Bar>echo "void glTexGendv(GLenum coord, GLenum pname, const GLdouble *params)"<Bar>echoh None<cr>
inorea glTexGenfv glTexGenfv<c-o>:echoh HintHL<Bar>echo "void glTexGenfv(GLenum coord, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glTexGeniv glTexGeniv<c-o>:echoh HintHL<Bar>echo "void glTexGeniv(GLenum coord, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glGetTexGendv glGetTexGendv<c-o>:echoh HintHL<Bar>echo "void glGetTexGendv(GLenum coord, GLenum pname, GLdouble *params)"<Bar>echoh None<cr>
inorea glGetTexGenfv glGetTexGenfv<c-o>:echoh HintHL<Bar>echo "void glGetTexGenfv(GLenum coord, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexGeniv glGetTexGeniv<c-o>:echoh HintHL<Bar>echo "void glGetTexGeniv(GLenum coord, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glTexEnvf glTexEnvf<c-o>:echoh HintHL<Bar>echo "void glTexEnvf(GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glTexEnvi glTexEnvi<c-o>:echoh HintHL<Bar>echo "void glTexEnvi(GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glTexEnvfv glTexEnvfv<c-o>:echoh HintHL<Bar>echo "void glTexEnvfv(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glTexEnviv glTexEnviv<c-o>:echoh HintHL<Bar>echo "void glTexEnviv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glGetTexEnvfv glGetTexEnvfv<c-o>:echoh HintHL<Bar>echo "void glGetTexEnvfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexEnviv glGetTexEnviv<c-o>:echoh HintHL<Bar>echo "void glGetTexEnviv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glTexParameterf glTexParameterf<c-o>:echoh HintHL<Bar>echo "void glTexParameterf(GLenum target, GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glTexParameteri glTexParameteri<c-o>:echoh HintHL<Bar>echo "void glTexParameteri(GLenum target, GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glTexParameterfv glTexParameterfv<c-o>:echoh HintHL<Bar>echo "void glTexParameterfv(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glTexParameteriv glTexParameteriv<c-o>:echoh HintHL<Bar>echo "void glTexParameteriv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glGetTexParameterfv glGetTexParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetTexParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexParameteriv glGetTexParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetTexParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glGetTexLevelParameterfv glGetTexLevelParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetTexLevelParameteriv glGetTexLevelParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glTexImage1D glTexImage1D<c-o>:echoh HintHL<Bar>echo "void glTexImage1D(GLenum target, GLint level, GLint internalFormat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glTexImage2D glTexImage2D<c-o>:echoh HintHL<Bar>echo "void glTexImage2D(GLenum target, GLint level, GLint internalFormat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glGetTexImage glGetTexImage<c-o>:echoh HintHL<Bar>echo "void glGetTexImage(GLenum target, GLint level, GLenum format, GLenum type, GLvoid *img)"<Bar>echoh None<cr>
inorea glGenTextures glGenTextures<c-o>:echoh HintHL<Bar>echo "void glGenTextures(GLsizei n, GLuint *textures)"<Bar>echoh None<cr>
inorea glDeleteTextures glDeleteTextures<c-o>:echoh HintHL<Bar>echo "void glDeleteTextures(GLsizei n, const GLuint *textures)"<Bar>echoh None<cr>
inorea glBindTexture glBindTexture<c-o>:echoh HintHL<Bar>echo "void glBindTexture(GLenum target, GLuint texture)"<Bar>echoh None<cr>
inorea glPrioritizeTextures glPrioritizeTextures<c-o>:echoh HintHL<Bar>echo "void glPrioritizeTextures(GLsizei n, const GLuint *textures, const GLclampf *priorities)"<Bar>echoh None<cr>
inorea glAreTexturesResident glAreTexturesResident<c-o>:echoh HintHL<Bar>echo "GLboolean glAreTexturesResident(GLsizei n, const GLuint *textures, GLboolean *residences)"<Bar>echoh None<cr>
inorea glIsTexture glIsTexture<c-o>:echoh HintHL<Bar>echo "GLboolean glIsTexture(GLuint texture)"<Bar>echoh None<cr>
inorea glTexSubImage1D glTexSubImage1D<c-o>:echoh HintHL<Bar>echo "void glTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glTexSubImage2D glTexSubImage2D<c-o>:echoh HintHL<Bar>echo "void glTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCopyTexImage1D glCopyTexImage1D<c-o>:echoh HintHL<Bar>echo "void glCopyTexImage1D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border)"<Bar>echoh None<cr>
inorea glCopyTexImage2D glCopyTexImage2D<c-o>:echoh HintHL<Bar>echo "void glCopyTexImage2D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)"<Bar>echoh None<cr>
inorea glCopyTexSubImage1D glCopyTexSubImage1D<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyTexSubImage2D glCopyTexSubImage2D<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glMap1d glMap1d<c-o>:echoh HintHL<Bar>echo "void glMap1d(GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points)"<Bar>echoh None<cr>
inorea glMap1f glMap1f<c-o>:echoh HintHL<Bar>echo "void glMap1f(GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points)"<Bar>echoh None<cr>
inorea glMap2d glMap2d<c-o>:echoh HintHL<Bar>echo "void glMap2d(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points)"<Bar>echoh None<cr>
inorea glMap2f glMap2f<c-o>:echoh HintHL<Bar>echo "void glMap2f(GLenum target, GLfloat  u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points)"<Bar>echoh None<cr>
inorea glGetMapdv glGetMapdv<c-o>:echoh HintHL<Bar>echo "void glGetMapdv(GLenum target, GLenum query, GLdouble *v)"<Bar>echoh None<cr>
inorea glGetMapfv glGetMapfv<c-o>:echoh HintHL<Bar>echo "void glGetMapfv(GLenum target, GLenum query, GLfloat *v)"<Bar>echoh None<cr>
inorea glGetMapiv glGetMapiv<c-o>:echoh HintHL<Bar>echo "void glGetMapiv(GLenum target, GLenum query, GLint *v)"<Bar>echoh None<cr>
inorea glEvalCoord1d glEvalCoord1d<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1d(GLdouble u)"<Bar>echoh None<cr>
inorea glEvalCoord1f glEvalCoord1f<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1f(GLfloat u)"<Bar>echoh None<cr>
inorea glEvalCoord1dv glEvalCoord1dv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1dv(const GLdouble *u)"<Bar>echoh None<cr>
inorea glEvalCoord1fv glEvalCoord1fv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord1fv(const GLfloat *u)"<Bar>echoh None<cr>
inorea glEvalCoord2d glEvalCoord2d<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2d(GLdouble u, GLdouble v)"<Bar>echoh None<cr>
inorea glEvalCoord2f glEvalCoord2f<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2f(GLfloat u, GLfloat v)"<Bar>echoh None<cr>
inorea glEvalCoord2dv glEvalCoord2dv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2dv(const GLdouble *u)"<Bar>echoh None<cr>
inorea glEvalCoord2fv glEvalCoord2fv<c-o>:echoh HintHL<Bar>echo "void glEvalCoord2fv(const GLfloat *u)"<Bar>echoh None<cr>
inorea glMapGrid1d glMapGrid1d<c-o>:echoh HintHL<Bar>echo "void glMapGrid1d(GLint un, GLdouble u1, GLdouble u2)"<Bar>echoh None<cr>
inorea glMapGrid1f glMapGrid1f<c-o>:echoh HintHL<Bar>echo "void glMapGrid1f(GLint un, GLfloat u1, GLfloat u2)"<Bar>echoh None<cr>
inorea glMapGrid2d glMapGrid2d<c-o>:echoh HintHL<Bar>echo "void glMapGrid2d(GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2)"<Bar>echoh None<cr>
inorea glMapGrid2f glMapGrid2f<c-o>:echoh HintHL<Bar>echo "void glMapGrid2f(GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2)"<Bar>echoh None<cr>
inorea glEvalPoint1 glEvalPoint1<c-o>:echoh HintHL<Bar>echo "void glEvalPoint1(GLint i)"<Bar>echoh None<cr>
inorea glEvalPoint2 glEvalPoint2<c-o>:echoh HintHL<Bar>echo "void glEvalPoint2(GLint i, GLint j)"<Bar>echoh None<cr>
inorea glEvalMesh1 glEvalMesh1<c-o>:echoh HintHL<Bar>echo "void glEvalMesh1(GLenum mode, GLint i1, GLint i2)"<Bar>echoh None<cr>
inorea glEvalMesh2 glEvalMesh2<c-o>:echoh HintHL<Bar>echo "void glEvalMesh2(GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2)"<Bar>echoh None<cr>
inorea glFogf glFogf<c-o>:echoh HintHL<Bar>echo "void glFogf(GLenum pname, GLfloat param)"<Bar>echoh None<cr>
inorea glFogi glFogi<c-o>:echoh HintHL<Bar>echo "void glFogi(GLenum pname, GLint param)"<Bar>echoh None<cr>
inorea glFogfv glFogfv<c-o>:echoh HintHL<Bar>echo "void glFogfv(GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glFogiv glFogiv<c-o>:echoh HintHL<Bar>echo "void glFogiv(GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glFeedbackBuffer glFeedbackBuffer<c-o>:echoh HintHL<Bar>echo "void glFeedbackBuffer(GLsizei size, GLenum type, GLfloat *buffer)"<Bar>echoh None<cr>
inorea glPassThrough glPassThrough<c-o>:echoh HintHL<Bar>echo "void glPassThrough(GLfloat token)"<Bar>echoh None<cr>
inorea glSelectBuffer glSelectBuffer<c-o>:echoh HintHL<Bar>echo "void glSelectBuffer(GLsizei size, GLuint *buffer)"<Bar>echoh None<cr>
inorea glInitNames glInitNames<c-o>:echoh HintHL<Bar>echo "void glInitNames(void)"<Bar>echoh None<cr>
inorea glLoadName glLoadName<c-o>:echoh HintHL<Bar>echo "void glLoadName(GLuint name)"<Bar>echoh None<cr>
inorea glPushName glPushName<c-o>:echoh HintHL<Bar>echo "void glPushName(GLuint name)"<Bar>echoh None<cr>
inorea glPopName glPopName<c-o>:echoh HintHL<Bar>echo "void glPopName(void)"<Bar>echoh None<cr>
inorea glDrawRangeElements glDrawRangeElements<c-o>:echoh HintHL<Bar>echo "void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices)"<Bar>echoh None<cr>
inorea glTexImage3D glTexImage3D<c-o>:echoh HintHL<Bar>echo "void glTexImage3D(GLenum target, GLint level, GLint internalFormat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glTexSubImage3D glTexSubImage3D<c-o>:echoh HintHL<Bar>echo "void glTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCopyTexSubImage3D glCopyTexSubImage3D<c-o>:echoh HintHL<Bar>echo "void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glColorTable glColorTable<c-o>:echoh HintHL<Bar>echo "void glColorTable(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glColorSubTable glColorSubTable<c-o>:echoh HintHL<Bar>echo "void glColorSubTable(GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glColorTableParameteriv glColorTableParameteriv<c-o>:echoh HintHL<Bar>echo "void glColorTableParameteriv(GLenum target, GLenum pname, const GLfloat *params)"<Bar>echoh None<cr>
inorea glColorTableParameterfv glColorTableParameterfv<c-o>:echoh HintHL<Bar>echo "void glColorTableParameterfv(GLenum target, GLenum pname, const GLint *params)"<Bar>echoh None<cr>
inorea glCopyColorSubTable glCopyColorSubTable<c-o>:echoh HintHL<Bar>echo "void glCopyColorSubTable(GLenum target, GLsizei start, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyColorTable glCopyColorTable<c-o>:echoh HintHL<Bar>echo "void glCopyColorTable(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glGetColorTable glGetColorTable<c-o>:echoh HintHL<Bar>echo "void glGetColorTable(GLenum target, GLenum format, GLenum type, GLvoid *table)"<Bar>echoh None<cr>
inorea glGetColorTableParameterfv glGetColorTableParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetColorTableParameteriv glGetColorTableParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetColorTableParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glBlendEquation glBlendEquation<c-o>:echoh HintHL<Bar>echo "void glBlendEquation(GLenum mode)"<Bar>echoh None<cr>
inorea glBlendColor glBlendColor<c-o>:echoh HintHL<Bar>echo "void glBlendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)"<Bar>echoh None<cr>
inorea glHistogram glHistogram<c-o>:echoh HintHL<Bar>echo "void glHistogram(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink)"<Bar>echoh None<cr>
inorea glResetHistogram glResetHistogram<c-o>:echoh HintHL<Bar>echo "void glResetHistogram(GLenum target)"<Bar>echoh None<cr>
inorea glGetHistogram glGetHistogram<c-o>:echoh HintHL<Bar>echo "void glGetHistogram(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetHistogramParameterfv glGetHistogramParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetHistogramParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetHistogramParameteriv glGetHistogramParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glMinmax glMinmax<c-o>:echoh HintHL<Bar>echo "void glMinmax(GLenum target, GLenum internalformat, GLboolean sink)"<Bar>echoh None<cr>
inorea glResetMinmax glResetMinmax<c-o>:echoh HintHL<Bar>echo "void glResetMinmax(GLenum target)"<Bar>echoh None<cr>
inorea glGetMinmax glGetMinmax<c-o>:echoh HintHL<Bar>echo "void glGetMinmax(GLenum target, GLboolean reset, GLenum format, GLenum types, GLvoid *values)"<Bar>echoh None<cr>
inorea glGetMinmaxParameterfv glGetMinmaxParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetMinmaxParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetMinmaxParameteriv glGetMinmaxParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glConvolutionFilter1D glConvolutionFilter1D<c-o>:echoh HintHL<Bar>echo "void glConvolutionFilter1D(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glConvolutionFilter2D glConvolutionFilter2D<c-o>:echoh HintHL<Bar>echo "void glConvolutionFilter2D(GLenum target,GLenum  internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *data)"<Bar>echoh None<cr>
inorea glConvolutionParameterf glConvolutionParameterf<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameterf(GLenum target, GLenum pname, GLfloat params)"<Bar>echoh None<cr>
inorea glConvolutionParameterfv glConvolutionParameterfv<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glConvolutionParameteri glConvolutionParameteri<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameteri(GLenum target, GLenum pname, GLint params)"<Bar>echoh None<cr>
inorea glConvolutionParameteriv glConvolutionParameteriv<c-o>:echoh HintHL<Bar>echo "void glConvolutionParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glCopyConvolutionFilter1D glCopyConvolutionFilter1D<c-o>:echoh HintHL<Bar>echo "void glCopyConvolutionFilter1D(GLenum target, GLint x, GLint y, GLsizei width)"<Bar>echoh None<cr>
inorea glCopyConvolutionFilter2D glCopyConvolutionFilter2D<c-o>:echoh HintHL<Bar>echo "void glCopyConvolutionFilter2D(GLenum target, GLint x, GLint y, GLsizei width, GLsizei height)"<Bar>echoh None<cr>
inorea glGetConvolutionFilter glGetConvolutionFilter<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionFilter(GLenum target, GLenum format, GLenum type, GLvoid *image)"<Bar>echoh None<cr>
inorea glGetConvolutionParameterfv glGetConvolutionParameterfv<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionParameterfv(GLenum target, GLenum pname, GLfloat *params)"<Bar>echoh None<cr>
inorea glGetConvolutionParameteriv glGetConvolutionParameteriv<c-o>:echoh HintHL<Bar>echo "void glGetConvolutionParameteriv(GLenum target, GLenum pname, GLint *params)"<Bar>echoh None<cr>
inorea glSeparableFilter2D glSeparableFilter2D<c-o>:echoh HintHL<Bar>echo "void glSeparableFilter2D(GLenum target, GLenum  internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *row, const GLvoid *column)"<Bar>echoh None<cr>
inorea glGetSeparableFilter glGetSeparableFilter<c-o>:echoh HintHL<Bar>echo "void glGetSeparableFilter(GLenum target, GLenum format, GLenum  type, GLvoid *row, GLvoid *column, GLvoid *span)"<Bar>echoh None<cr>
inorea glActiveTexture glActiveTexture<c-o>:echoh HintHL<Bar>echo "void glActiveTexture(GLenum texture)"<Bar>echoh None<cr>
inorea glClientActiveTexture glClientActiveTexture<c-o>:echoh HintHL<Bar>echo "void glClientActiveTexture(GLenum texture)"<Bar>echoh None<cr>
inorea glCompressedTexImage1D glCompressedTexImage1D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage1D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexImage2D glCompressedTexImage2D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexImage3D glCompressedTexImage3D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage1D glCompressedTexSubImage1D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage2D glCompressedTexSubImage2D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glCompressedTexSubImage3D glCompressedTexSubImage3D<c-o>:echoh HintHL<Bar>echo "void glCompressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *data)"<Bar>echoh None<cr>
inorea glGetCompressedTexImage glGetCompressedTexImage<c-o>:echoh HintHL<Bar>echo "void glGetCompressedTexImage(GLenum target, GLint lod, GLvoid *img)"<Bar>echoh None<cr>
inorea glMultiTexCoord1d glMultiTexCoord1d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1d(GLenum target, GLdouble s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1dv glMultiTexCoord1dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1f glMultiTexCoord1f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1f(GLenum target, GLfloat s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1fv glMultiTexCoord1fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1i glMultiTexCoord1i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1i(GLenum target, GLint s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1iv glMultiTexCoord1iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1s glMultiTexCoord1s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1s(GLenum target, GLshort s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1sv glMultiTexCoord1sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2d glMultiTexCoord2d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2d(GLenum target, GLdouble s, GLdouble t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2dv glMultiTexCoord2dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2f glMultiTexCoord2f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2f(GLenum target, GLfloat s, GLfloat t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2fv glMultiTexCoord2fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2i glMultiTexCoord2i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2i(GLenum target, GLint s, GLint t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2iv glMultiTexCoord2iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2s glMultiTexCoord2s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2s(GLenum target, GLshort s, GLshort t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2sv glMultiTexCoord2sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3d glMultiTexCoord3d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3d(GLenum target, GLdouble s, GLdouble t, GLdouble r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3dv glMultiTexCoord3dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3f glMultiTexCoord3f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3f(GLenum target, GLfloat s, GLfloat t, GLfloat r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3fv glMultiTexCoord3fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3i glMultiTexCoord3i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3i(GLenum target, GLint s, GLint t, GLint r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3iv glMultiTexCoord3iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3s glMultiTexCoord3s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3s(GLenum target, GLshort s, GLshort t, GLshort r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3sv glMultiTexCoord3sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4d glMultiTexCoord4d<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4d(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4dv glMultiTexCoord4dv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4dv(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4f glMultiTexCoord4f<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4f(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4fv glMultiTexCoord4fv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4fv(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4i glMultiTexCoord4i<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4i(GLenum target, GLint s, GLint t, GLint r, GLint q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4iv glMultiTexCoord4iv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4iv(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4s glMultiTexCoord4s<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4s(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4sv glMultiTexCoord4sv<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4sv(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glLoadTransposeMatrixd glLoadTransposeMatrixd<c-o>:echoh HintHL<Bar>echo "void glLoadTransposeMatrixd(const GLdouble m[16])"<Bar>echoh None<cr>
inorea glLoadTransposeMatrixf glLoadTransposeMatrixf<c-o>:echoh HintHL<Bar>echo "void glLoadTransposeMatrixf(const GLfloat m[16])"<Bar>echoh None<cr>
inorea glMultTransposeMatrixd glMultTransposeMatrixd<c-o>:echoh HintHL<Bar>echo "void glMultTransposeMatrixd(const GLdouble m[16])"<Bar>echoh None<cr>
inorea glMultTransposeMatrixf glMultTransposeMatrixf<c-o>:echoh HintHL<Bar>echo "void glMultTransposeMatrixf(const GLfloat m[16])"<Bar>echoh None<cr>
inorea glSampleCoverage glSampleCoverage<c-o>:echoh HintHL<Bar>echo "void glSampleCoverage(GLclampf value, GLboolean invert)"<Bar>echoh None<cr>
inorea glActiveTextureARB glActiveTextureARB<c-o>:echoh HintHL<Bar>echo "void glActiveTextureARB(GLenum texture)"<Bar>echoh None<cr>
inorea glClientActiveTextureARB glClientActiveTextureARB<c-o>:echoh HintHL<Bar>echo "void glClientActiveTextureARB(GLenum texture)"<Bar>echoh None<cr>
inorea glMultiTexCoord1dARB glMultiTexCoord1dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1dARB(GLenum target, GLdouble s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1dvARB glMultiTexCoord1dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1fARB glMultiTexCoord1fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1fARB(GLenum target, GLfloat s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1fvARB glMultiTexCoord1fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1iARB glMultiTexCoord1iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1iARB(GLenum target, GLint s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1ivARB glMultiTexCoord1ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord1sARB glMultiTexCoord1sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1sARB(GLenum target, GLshort s)"<Bar>echoh None<cr>
inorea glMultiTexCoord1svARB glMultiTexCoord1svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord1svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2dARB glMultiTexCoord2dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2dARB(GLenum target, GLdouble s, GLdouble t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2dvARB glMultiTexCoord2dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2fARB glMultiTexCoord2fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2fARB(GLenum target, GLfloat s, GLfloat t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2fvARB glMultiTexCoord2fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2iARB glMultiTexCoord2iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2iARB(GLenum target, GLint s, GLint t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2ivARB glMultiTexCoord2ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord2sARB glMultiTexCoord2sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2sARB(GLenum target, GLshort s, GLshort t)"<Bar>echoh None<cr>
inorea glMultiTexCoord2svARB glMultiTexCoord2svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord2svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3dARB glMultiTexCoord3dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3dvARB glMultiTexCoord3dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3fARB glMultiTexCoord3fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3fvARB glMultiTexCoord3fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3iARB glMultiTexCoord3iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3iARB(GLenum target, GLint s, GLint t, GLint r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3ivARB glMultiTexCoord3ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord3sARB glMultiTexCoord3sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3sARB(GLenum target, GLshort s, GLshort t, GLshort r)"<Bar>echoh None<cr>
inorea glMultiTexCoord3svARB glMultiTexCoord3svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord3svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4dARB glMultiTexCoord4dARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4dvARB glMultiTexCoord4dvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4dvARB(GLenum target, const GLdouble *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4fARB glMultiTexCoord4fARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4fvARB glMultiTexCoord4fvARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4fvARB(GLenum target, const GLfloat *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4iARB glMultiTexCoord4iARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4iARB(GLenum target, GLint s, GLint t, GLint r, GLint q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4ivARB glMultiTexCoord4ivARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4ivARB(GLenum target, const GLint *v)"<Bar>echoh None<cr>
inorea glMultiTexCoord4sARB glMultiTexCoord4sARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4sARB(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q)"<Bar>echoh None<cr>
inorea glMultiTexCoord4svARB glMultiTexCoord4svARB<c-o>:echoh HintHL<Bar>echo "void glMultiTexCoord4svARB(GLenum target, const GLshort *v)"<Bar>echoh None<cr>
inorea glCreateDebugObjectMESA glCreateDebugObjectMESA<c-o>:echoh HintHL<Bar>echo "GLhandleARB glCreateDebugObjectMESA(void)"<Bar>echoh None<cr>
inorea glClearDebugLogMESA glClearDebugLogMESA<c-o>:echoh HintHL<Bar>echo "void glClearDebugLogMESA(GLhandleARB obj, GLenum logType, GLenum shaderType)"<Bar>echoh None<cr>
"void glGetDebugLogMESA (GLhandleARB obj, GLenum logType, GLenum shaderType, GLsizei maxLength,
"GLsizei glGetDebugLogLengthMESA (GLhandleARB obj, GLenum logType, GLenum shaderType)
inorea glProgramCallbackMESA glProgramCallbackMESA<c-o>:echoh HintHL<Bar>echo "void glProgramCallbackMESA(GLenum target, GLprogramcallbackMESA callback, GLvoid *data)"<Bar>echoh None<cr>
inorea glGetProgramRegisterfvMESA glGetProgramRegisterfvMESA<c-o>:echoh HintHL<Bar>echo "void glGetProgramRegisterfvMESA(GLenum target, GLsizei len, const GLubyte *name, GLfloat *v)"<Bar>echoh None<cr>
"void glFramebufferTextureLayerEXT (GLenum target,
inorea glBlendEquationSeparateATI glBlendEquationSeparateATI<c-o>:echoh HintHL<Bar>echo "void glBlendEquationSeparateATI(GLenum modeRGB, GLenum modeA)"<Bar>echoh None<cr>
inorea glEGLImageTargetTexture2DOES glEGLImageTargetTexture2DOES<c-o>:echoh HintHL<Bar>echo "void glEGLImageTargetTexture2DOES(GLenum target, GLeglImageOES image)"<Bar>echoh None<cr>
inorea glEGLImageTargetRenderbufferStorageOES glEGLImageTargetRenderbufferStorageOES<c-o>:echoh HintHL<Bar>echo "void glEGLImageTargetRenderbufferStorageOES(GLenum target, GLeglImageOES image)"<Bar>echoh None<cr>
inorea gluBeginCurve gluBeginCurve<c-o>:echoh HintHL<Bar>echo "void gluBeginCurve(GLUnurbs* nurb)"<Bar>echoh None<cr>
inorea gluBeginPolygon gluBeginPolygon<c-o>:echoh HintHL<Bar>echo "void gluBeginPolygon(GLUtesselator* tess)"<Bar>echoh None<cr>
inorea gluBeginSurface gluBeginSurface<c-o>:echoh HintHL<Bar>echo "void gluBeginSurface(GLUnurbs* nurb)"<Bar>echoh None<cr>
inorea gluBeginTrim gluBeginTrim<c-o>:echoh HintHL<Bar>echo "void gluBeginTrim(GLUnurbs* nurb)"<Bar>echoh None<cr>
inorea gluBuild1DMipmapLevels gluBuild1DMipmapLevels<c-o>:echoh HintHL<Bar>echo "GLint gluBuild1DMipmapLevels(GLenum target, GLint internalFormat, GLsizei width, GLenum format, GLenum type, GLint level, GLint base, GLint max, const void *data)"<Bar>echoh None<cr>
inorea gluBuild1DMipmaps gluBuild1DMipmaps<c-o>:echoh HintHL<Bar>echo "GLint gluBuild1DMipmaps(GLenum target, GLint internalFormat, GLsizei width, GLenum format, GLenum type, const void *data)"<Bar>echoh None<cr>
inorea gluBuild2DMipmapLevels gluBuild2DMipmapLevels<c-o>:echoh HintHL<Bar>echo "GLint gluBuild2DMipmapLevels(GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLenum format, GLenum type, GLint level, GLint base, GLint max, const void *data)"<Bar>echoh None<cr>
inorea gluBuild2DMipmaps gluBuild2DMipmaps<c-o>:echoh HintHL<Bar>echo "GLint gluBuild2DMipmaps(GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *data)"<Bar>echoh None<cr>
inorea gluBuild3DMipmapLevels gluBuild3DMipmapLevels<c-o>:echoh HintHL<Bar>echo "GLint gluBuild3DMipmapLevels(GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLint level, GLint base, GLint max, const void *data)"<Bar>echoh None<cr>
inorea gluBuild3DMipmaps gluBuild3DMipmaps<c-o>:echoh HintHL<Bar>echo "GLint gluBuild3DMipmaps(GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *data)"<Bar>echoh None<cr>
inorea gluCheckExtension gluCheckExtension<c-o>:echoh HintHL<Bar>echo "GLboolean gluCheckExtension(const GLubyte *extName, const GLubyte *extString)"<Bar>echoh None<cr>
inorea gluCylinder gluCylinder<c-o>:echoh HintHL<Bar>echo "void gluCylinder(GLUquadric* quad, GLdouble base, GLdouble top, GLdouble height, GLint slices, GLint stacks)"<Bar>echoh None<cr>
inorea gluDeleteNurbsRenderer gluDeleteNurbsRenderer<c-o>:echoh HintHL<Bar>echo "void gluDeleteNurbsRenderer(GLUnurbs* nurb)"<Bar>echoh None<cr>
inorea gluDeleteQuadric gluDeleteQuadric<c-o>:echoh HintHL<Bar>echo "void gluDeleteQuadric(GLUquadric* quad)"<Bar>echoh None<cr>
inorea gluDeleteTess gluDeleteTess<c-o>:echoh HintHL<Bar>echo "void gluDeleteTess(GLUtesselator* tess)"<Bar>echoh None<cr>
inorea gluDisk gluDisk<c-o>:echoh HintHL<Bar>echo "void gluDisk(GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, GLint loops)"<Bar>echoh None<cr>
inorea gluEndCurve gluEndCurve<c-o>:echoh HintHL<Bar>echo "void gluEndCurve(GLUnurbs* nurb)"<Bar>echoh None<cr>
inorea gluEndPolygon gluEndPolygon<c-o>:echoh HintHL<Bar>echo "void gluEndPolygon(GLUtesselator* tess)"<Bar>echoh None<cr>
inorea gluEndSurface gluEndSurface<c-o>:echoh HintHL<Bar>echo "void gluEndSurface(GLUnurbs* nurb)"<Bar>echoh None<cr>
inorea gluEndTrim gluEndTrim<c-o>:echoh HintHL<Bar>echo "void gluEndTrim(GLUnurbs* nurb)"<Bar>echoh None<cr>
inorea gluErrorString gluErrorString<c-o>:echoh HintHL<Bar>echo "const GLubyte *gluErrorString(GLenum error)"<Bar>echoh None<cr>
inorea gluGetNurbsProperty gluGetNurbsProperty<c-o>:echoh HintHL<Bar>echo "void gluGetNurbsProperty(GLUnurbs* nurb, GLenum property, GLfloat* data)"<Bar>echoh None<cr>
inorea gluGetString gluGetString<c-o>:echoh HintHL<Bar>echo "const GLubyte *gluGetString(GLenum name)"<Bar>echoh None<cr>
inorea gluGetTessProperty gluGetTessProperty<c-o>:echoh HintHL<Bar>echo "void gluGetTessProperty(GLUtesselator* tess, GLenum which, GLdouble* data)"<Bar>echoh None<cr>
inorea gluLoadSamplingMatrices gluLoadSamplingMatrices<c-o>:echoh HintHL<Bar>echo "void gluLoadSamplingMatrices(GLUnurbs* nurb, const GLfloat *model, const GLfloat *perspective, const GLint *view)"<Bar>echoh None<cr>
inorea gluLookAt gluLookAt<c-o>:echoh HintHL<Bar>echo "void gluLookAt(GLdouble eyeX, GLdouble eyeY, GLdouble eyeZ, GLdouble centerX, GLdouble centerY, GLdouble centerZ, GLdouble upX, GLdouble upY, GLdouble upZ)"<Bar>echoh None<cr>
inorea gluNewNurbsRenderer gluNewNurbsRenderer<c-o>:echoh HintHL<Bar>echo "GLUnurbs *gluNewNurbsRenderer(void)"<Bar>echoh None<cr>
inorea gluNewQuadric gluNewQuadric<c-o>:echoh HintHL<Bar>echo "GLUquadric *gluNewQuadric(void)"<Bar>echoh None<cr>
inorea gluNewTess gluNewTess<c-o>:echoh HintHL<Bar>echo "GLUtesselator *gluNewTess(void)"<Bar>echoh None<cr>
inorea gluNextContour gluNextContour<c-o>:echoh HintHL<Bar>echo "void gluNextContour(GLUtesselator* tess, GLenum type)"<Bar>echoh None<cr>
inorea gluNurbsCallback gluNurbsCallback<c-o>:echoh HintHL<Bar>echo "void gluNurbsCallback(GLUnurbs* nurb, GLenum which, _GLUfuncptr CallBackFunc)"<Bar>echoh None<cr>
inorea gluNurbsCallbackData gluNurbsCallbackData<c-o>:echoh HintHL<Bar>echo "void gluNurbsCallbackData(GLUnurbs* nurb, GLvoid* userData)"<Bar>echoh None<cr>
inorea gluNurbsCallbackDataEXT gluNurbsCallbackDataEXT<c-o>:echoh HintHL<Bar>echo "void gluNurbsCallbackDataEXT(GLUnurbs* nurb, GLvoid* userData)"<Bar>echoh None<cr>
inorea gluNurbsCurve gluNurbsCurve<c-o>:echoh HintHL<Bar>echo "void gluNurbsCurve(GLUnurbs* nurb, GLint knotCount, GLfloat *knots, GLint stride, GLfloat *control, GLint order, GLenum type)"<Bar>echoh None<cr>
inorea gluNurbsProperty gluNurbsProperty<c-o>:echoh HintHL<Bar>echo "void gluNurbsProperty(GLUnurbs* nurb, GLenum property, GLfloat value)"<Bar>echoh None<cr>
inorea gluNurbsSurface gluNurbsSurface<c-o>:echoh HintHL<Bar>echo "void gluNurbsSurface(GLUnurbs* nurb, GLint sKnotCount, GLfloat* sKnots, GLint tKnotCount, GLfloat* tKnots, GLint sStride, GLint tStride, GLfloat* control, GLint sOrder, GLint tOrder, GLenum type)"<Bar>echoh None<cr>
inorea gluOrtho2D gluOrtho2D<c-o>:echoh HintHL<Bar>echo "void gluOrtho2D(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top)"<Bar>echoh None<cr>
inorea gluPartialDisk gluPartialDisk<c-o>:echoh HintHL<Bar>echo "void gluPartialDisk(GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, GLint loops, GLdouble start, GLdouble sweep)"<Bar>echoh None<cr>
inorea gluPerspective gluPerspective<c-o>:echoh HintHL<Bar>echo "void gluPerspective(GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar)"<Bar>echoh None<cr>
inorea gluPickMatrix gluPickMatrix<c-o>:echoh HintHL<Bar>echo "void gluPickMatrix(GLdouble x, GLdouble y, GLdouble delX, GLdouble delY, GLint *viewport)"<Bar>echoh None<cr>
inorea gluProject gluProject<c-o>:echoh HintHL<Bar>echo "GLint gluProject(GLdouble objX, GLdouble objY, GLdouble objZ, const GLdouble *model, const GLdouble *proj, const GLint *view, GLdouble* winX, GLdouble* winY, GLdouble* winZ)"<Bar>echoh None<cr>
inorea gluPwlCurve gluPwlCurve<c-o>:echoh HintHL<Bar>echo "void gluPwlCurve(GLUnurbs* nurb, GLint count, GLfloat* data, GLint stride, GLenum type)"<Bar>echoh None<cr>
inorea gluQuadricCallback gluQuadricCallback<c-o>:echoh HintHL<Bar>echo "void gluQuadricCallback(GLUquadric* quad, GLenum which, _GLUfuncptr CallBackFunc)"<Bar>echoh None<cr>
inorea gluQuadricDrawStyle gluQuadricDrawStyle<c-o>:echoh HintHL<Bar>echo "void gluQuadricDrawStyle(GLUquadric* quad, GLenum draw)"<Bar>echoh None<cr>
inorea gluQuadricNormals gluQuadricNormals<c-o>:echoh HintHL<Bar>echo "void gluQuadricNormals(GLUquadric* quad, GLenum normal)"<Bar>echoh None<cr>
inorea gluQuadricOrientation gluQuadricOrientation<c-o>:echoh HintHL<Bar>echo "void gluQuadricOrientation(GLUquadric* quad, GLenum orientation)"<Bar>echoh None<cr>
inorea gluQuadricTexture gluQuadricTexture<c-o>:echoh HintHL<Bar>echo "void gluQuadricTexture(GLUquadric* quad, GLboolean texture)"<Bar>echoh None<cr>
inorea gluScaleImage gluScaleImage<c-o>:echoh HintHL<Bar>echo "GLint gluScaleImage(GLenum format, GLsizei wIn, GLsizei hIn, GLenum typeIn, const void *dataIn, GLsizei wOut, GLsizei hOut, GLenum typeOut, GLvoid* dataOut)"<Bar>echoh None<cr>
inorea gluSphere gluSphere<c-o>:echoh HintHL<Bar>echo "void gluSphere(GLUquadric* quad, GLdouble radius, GLint slices, GLint stacks)"<Bar>echoh None<cr>
inorea gluTessBeginContour gluTessBeginContour<c-o>:echoh HintHL<Bar>echo "void gluTessBeginContour(GLUtesselator* tess)"<Bar>echoh None<cr>
inorea gluTessBeginPolygon gluTessBeginPolygon<c-o>:echoh HintHL<Bar>echo "void gluTessBeginPolygon(GLUtesselator* tess, GLvoid* data)"<Bar>echoh None<cr>
inorea gluTessCallback gluTessCallback<c-o>:echoh HintHL<Bar>echo "void gluTessCallback(GLUtesselator* tess, GLenum which, _GLUfuncptr CallBackFunc)"<Bar>echoh None<cr>
inorea gluTessEndContour gluTessEndContour<c-o>:echoh HintHL<Bar>echo "void gluTessEndContour(GLUtesselator* tess)"<Bar>echoh None<cr>
inorea gluTessEndPolygon gluTessEndPolygon<c-o>:echoh HintHL<Bar>echo "void gluTessEndPolygon(GLUtesselator* tess)"<Bar>echoh None<cr>
inorea gluTessNormal gluTessNormal<c-o>:echoh HintHL<Bar>echo "void gluTessNormal(GLUtesselator* tess, GLdouble valueX, GLdouble valueY, GLdouble valueZ)"<Bar>echoh None<cr>
inorea gluTessProperty gluTessProperty<c-o>:echoh HintHL<Bar>echo "void gluTessProperty(GLUtesselator* tess, GLenum which, GLdouble data)"<Bar>echoh None<cr>
inorea gluTessVertex gluTessVertex<c-o>:echoh HintHL<Bar>echo "void gluTessVertex(GLUtesselator* tess, GLdouble *location, GLvoid* data)"<Bar>echoh None<cr>
inorea gluUnProject gluUnProject<c-o>:echoh HintHL<Bar>echo "GLint gluUnProject(GLdouble winX, GLdouble winY, GLdouble winZ, const GLdouble *model, const GLdouble *proj, const GLint *view, GLdouble* objX, GLdouble* objY, GLdouble* objZ)"<Bar>echoh None<cr>
inorea gluUnProject4 gluUnProject4<c-o>:echoh HintHL<Bar>echo "GLint gluUnProject4(GLdouble winX, GLdouble winY, GLdouble winZ, GLdouble clipW, const GLdouble *model, const GLdouble *proj, const GLint *view, GLdouble nearVal, GLdouble farVal, GLdouble* objX, GLdouble* objY, GLdouble* objZ, GLdouble* objW)"<Bar>echoh None<cr>
"inorea a64l	a64l<c-o>:echoh HintHL<Bar>echo "long a64l(char * str64)"<Bar>echoh None<cr>

" ---------------------------------------------------------------------
"  Highlighting Control: {{{1
" if the "HintHL" highlighting group hasn't been defined, then this function will define it
fun! s:HLTEST(hlname)
  let id_hlname= hlID(a:hlname)
  let fg_hlname= string(synIDattr(synIDtrans(hlID(a:hlname)),"fg"))
  if id_hlname == 0 || fg_hlname == "0" || fg_hlname == "-1"
   return 0
  endif
  return 1
endfun
if !s:HLTEST("HintHL")
 if &bg == "dark"
  hi HintHL ctermfg=blue ctermbg=white guifg=blue3 guibg=white
 else
  hi HintHL ctermfg=white ctermbg=blue guifg=white guibg=blue3
 endif
endif
delf s:HLTEST

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
" vim: ts=4 fdm=marker
