import React, { FC } from 'react';
import { Comment } from '../../../lib/astrid-tech-api';
import { CommentNode } from './CommentNode';

export type CommentListProps = {
  comments: Comment[];
  isReply?: boolean;
};

export const CommentList: FC<CommentListProps> = ({
  comments,
  isReply = false,
}) => {
  return (
    <>
      {comments.map((c, i) => {
        return <CommentNode key={c.id} comment={c} isReply={isReply} />;
      })}
    </>
  );
};
